;;; gptel-codex.el --- Codex backend for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Codex (ChatGPT OAuth) support via the ChatGPT backend responses API.

;;; Code:
(require 'cl-generic)
(eval-when-compile (require 'cl-lib))
(require 'gptel)
(require 'gptel-openai)
(require 'subr-x)

(defvar gptel-model)
(defvar gptel-stream)
(defvar gptel-use-curl)
(defvar gptel-use-tools)
(defvar gptel-tools)
(defvar gptel-backend)
(defvar gptel--request-params)

(declare-function gptel--merge-plists "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--model-request-params "gptel")
(declare-function gptel--process-models "gptel-openai")
(declare-function gptel--parse-tools "gptel-request")
(declare-function gptel-codex-auth-header "gptel-codex-auth")

(defgroup gptel-codex nil
  "Codex (ChatGPT OAuth) backend."
  :group 'gptel)

(defcustom gptel-codex-instructions 'auto
  "Instructions to send with Codex responses requests.

When set to the symbol `auto', gptel picks a model-appropriate
prompt from the bundled Codex prompt files.  When set to a
string, it is sent verbatim as the instructions payload.  You
can also supply a function that accepts the model name string and
returns an instructions string."
  :type '(choice
          (const :tag "Auto-select" auto)
          (string :tag "Custom instructions")
          (function :tag "Function"))
  :group 'gptel-codex)

(defcustom gptel-codex-originator "codex_cli_rs"
  "Originator header for Codex requests."
  :type 'string
  :group 'gptel-codex)

(defcustom gptel-codex-user-agent "codex-cli"
  "User-Agent header for Codex requests."
  :type 'string
  :group 'gptel-codex)

(defconst gptel-codex--prompt-subdir "codex-prompts"
  "Subdirectory within `gptel-codex--prompt-dir' with prompt files.")

(defvar gptel-codex--prompt-cache (make-hash-table :test 'equal)
  "Cache of Codex prompt file contents.")

(defun gptel-codex--prompt-base-dir ()
  (file-name-directory (or load-file-name buffer-file-name)))

(defun gptel-codex--prompt-repo-dir (dir)
  (when (and dir
             (string-match "/straight/build-[^/]+/gptel/?\\'" dir))
    (replace-regexp-in-string
     "/straight/build-[^/]+/gptel/?\\'"
     "/straight/repos/gptel/"
     dir)))

(defun gptel-codex--prompt-dirs ()
  (let* ((base (gptel-codex--prompt-base-dir))
         (repo (gptel-codex--prompt-repo-dir base))
         (roots (delete-dups (delq nil (list base repo)))))
    (cl-loop for root in roots
             for dir = (expand-file-name gptel-codex--prompt-subdir root)
             when (file-directory-p dir)
             collect dir)))

(defun gptel-codex--prompt-path (filename)
  (let ((candidates (cl-loop for dir in (gptel-codex--prompt-dirs)
                             collect (expand-file-name filename dir))))
    (or (cl-find-if #'file-readable-p candidates)
        (error "Missing Codex prompt file: %s (searched %s)"
               filename
               (if candidates
                   (mapconcat #'identity candidates ", ")
                 "no codex-prompts directories found")))))

(defun gptel-codex--read-prompt (filename)
  (or (gethash filename gptel-codex--prompt-cache)
      (let ((path (gptel-codex--prompt-path filename)))
        (let ((text (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
          (puthash filename text gptel-codex--prompt-cache)
          text))))

(defun gptel-codex--instructions-for-model (model-name)
  (cond
   ((string-prefix-p "gpt-5.2-codex" model-name)
    (gptel-codex--read-prompt "gpt-5.2-codex_prompt.md"))
   ((string-prefix-p "gpt-5.1-codex-max" model-name)
    (gptel-codex--read-prompt "gpt-5.1-codex-max_prompt.md"))
   ((or (string-prefix-p "gpt-5-codex" model-name)
        (string-prefix-p "gpt-5.1-codex" model-name)
        (string-prefix-p "codex-" model-name))
    (gptel-codex--read-prompt "gpt_5_codex_prompt.md"))
   (t (gptel-codex--read-prompt "gpt_5_codex_prompt.md"))))

(defun gptel-codex--resolve-instructions (model)
  (let ((model-name (gptel--model-name model)))
    (cond
     ((stringp gptel-codex-instructions) gptel-codex-instructions)
     ((functionp gptel-codex-instructions)
      (funcall gptel-codex-instructions model-name))
     (t (gptel-codex--instructions-for-model model-name)))))

(defun gptel-codex--content-item->input (item)
  (cond
   ((stringp item)
    (list :type "input_text" :text item))
   ((and (listp item) (plist-member item :type))
    (pcase (plist-get item :type)
      ("text"
       (let ((text (plist-get item :text)))
         (when text (list :type "input_text" :text text))))
      ("input_text"
       (let ((text (plist-get item :text)))
         (when text (list :type "input_text" :text text))))
      ("image_url"
       (let* ((image (plist-get item :image_url))
              (url (if (plistp image) (plist-get image :url) image)))
         (when url (list :type "input_image" :image_url url))))
      ("input_image"
       (let ((url (plist-get item :image_url)))
         (when url (list :type "input_image" :image_url url))))
      (_ nil)))
   (t nil)))

(defun gptel-codex--content->input (content)
  (cond
   ((or (null content) (eq content :null)) [])
   ((stringp content)
    (vector (list :type "input_text" :text content)))
   ((vectorp content)
    (vconcat (delq nil (mapcar #'gptel-codex--content-item->input
                               (append content nil)))))
   ((listp content)
    (if (plist-member content :type)
        (if-let* ((item (gptel-codex--content-item->input content)))
            (vector item)
          [])
      (vconcat (delq nil (mapcar #'gptel-codex--content-item->input content)))))
   (t
    (vector (list :type "input_text" :text (format "%s" content))))))

(defun gptel-codex--message->input (message)
  (let* ((role (plist-get message :role))
         (role (cond
                ((stringp role) role)
                ((symbolp role) (symbol-name role))
                (t "user")))
         (content (gptel-codex--content->input (plist-get message :content))))
    (list :type "message" :role role :content content)))

(defun gptel-codex--input-items (prompts)
  (vconcat
   (cl-loop for prompt in prompts
            for item = (gptel-codex--message->input prompt)
            when item collect item)))

(defun gptel-codex--extract-output-text (content)
  (let ((items (cond
                ((vectorp content) (append content nil))
                ((listp content) content)
                (t nil)))
        (texts nil))
    (dolist (item items)
      (when (and (listp item) (equal (plist-get item :type) "output_text"))
        (when-let* ((text (plist-get item :text)))
          (push text texts))))
    (when texts (apply #'concat (nreverse texts)))))

(defun gptel-codex--event-error (event)
  (let* ((response (plist-get event :response))
         (error-data (and (listp response) (plist-get response :error))))
    (cond
     ((stringp error-data) error-data)
     ((listp error-data)
      (or (plist-get error-data :message)
          (plist-get error-data :code)
          "Codex response failed"))
     (t nil))))

(defun gptel-codex--event-text (event info)
  (let ((kind (plist-get event :type)))
    (cond
     ((equal kind "response.created")
      (plist-put info :codex-seen-delta nil)
      nil)
     ((equal kind "response.output_text.delta")
      (let ((delta (plist-get event :delta)))
        (when (and (stringp delta) (not (string-empty-p delta)))
          (plist-put info :codex-seen-delta t)
          delta)))
     ((equal kind "response.output_item.done")
      (unless (plist-get info :codex-seen-delta)
        (when-let* ((item (plist-get event :item))
                    ((listp item))
                    ((equal (plist-get item :type) "message"))
                    (content (plist-get item :content))
                    (text (gptel-codex--extract-output-text content)))
          text)))
     ((equal kind "response.reasoning_text.delta")
      (let ((delta (plist-get event :delta)))
        (when (and (stringp delta) (not (string-empty-p delta)))
          (plist-put info :reasoning delta))
        nil))
     ((equal kind "response.reasoning_summary_text.delta")
      (let ((delta (plist-get event :delta)))
        (when (and (stringp delta) (not (string-empty-p delta)))
          (plist-put info :reasoning delta))
        nil))
     ((equal kind "response.failed")
      (when-let* ((message (gptel-codex--event-error event)))
        (plist-put info :error message))
      nil)
     (t nil))))

(defun gptel-codex--ensure-streaming ()
  (unless gptel-stream
    (error "Codex backend requires streaming; set gptel-stream to t and pass :stream t to gptel-request"))
  (unless gptel-use-curl
    (error "Codex backend requires curl streaming; set gptel-use-curl to t")))

(defun gptel-codex--force-stream-info (fsm)
  (let ((info (gptel-fsm-info fsm)))
    (when (cl-typep gptel-backend 'gptel-codex)
      (setq gptel-stream t
            gptel-use-curl t)
      (plist-put info :stream t))))

(with-eval-after-load 'gptel-request
  (advice-add 'gptel--realize-query :before #'gptel-codex--force-stream-info))

;;; Codex (Responses API)
(cl-defstruct (gptel-codex (:constructor gptel--make-codex)
                           (:copier nil)
                           (:include gptel-openai)))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-codex) info)
  (let ((content-strs)
        (pt (point)))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (setq pt (match-beginning 0))
          (when (equal (line-end-position) (point-max))
            (error "Data block incomplete"))
          (goto-char (match-end 0))
          (skip-chars-forward " \t")
          (cond
           ((looking-at "\\[DONE\\]") nil)
           ((looking-at "$") nil)
           (t
            (let ((event (gptel--json-read)))
              (when-let* ((text (gptel-codex--event-text event info)))
                (push text content-strs))))))
      (error (goto-char pt)))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--request-data ((backend gptel-codex) prompts)
  "JSON encode PROMPTS for Codex responses."
  (gptel-codex--ensure-streaming)
  (let* ((model-name (gptel--model-name gptel-model))
         (instructions (gptel-codex--resolve-instructions gptel-model))
         (input (gptel-codex--input-items prompts))
         (body `(:model ,model-name
                 :instructions ,instructions
                 :input ,input
                 :stream t)))
    (when (and gptel-use-tools gptel-tools)
      (plist-put body :tools (gptel--parse-tools backend gptel-tools))
      (plist-put body :tool_choice "auto")
      (plist-put body :parallel_tool_calls t))
    (gptel--merge-plists
     body
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params gptel-model))))

(cl-defmethod gptel--inject-prompt ((_backend gptel-codex) data new-prompt
                                   &optional _position)
  (when (keywordp (car-safe new-prompt))
    (setq new-prompt (list new-prompt)))
  (let ((items (gptel-codex--input-items new-prompt))
        (existing (plist-get data :input)))
    (plist-put data :input (vconcat existing items))))

(defun gptel-codex--default-header ()
  (unless (fboundp 'gptel-codex-auth-header)
    (error "gptel-codex-auth-header not available; require gptel-codex-auth"))
  (let ((headers (funcall #'gptel-codex-auth-header)))
    (when gptel-codex-originator
      (push (cons "originator" gptel-codex-originator) headers))
    (when gptel-codex-user-agent
      (push (cons "User-Agent" gptel-codex-user-agent) headers))
    (push (cons "Accept" "text/event-stream") headers)
    headers))

;;;###autoload
(cl-defun gptel-make-codex
    (name &key curl-args models (stream t) key request-params
          (header #'gptel-codex--default-header)
          (host "chatgpt.com")
          (protocol "https")
          (endpoint "/backend-api/codex/responses"))
  "Register a Codex backend for gptel with NAME.

STREAM must remain enabled; Codex responses require server-sent events."
  (declare (indent 1))
  (let ((backend (gptel--make-codex
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
            backend))))

(provide 'gptel-codex)
;;; gptel-codex.el ends here
