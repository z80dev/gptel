;;; gptel-codex-auth.el --- Codex OAuth helper for gptel -*- lexical-binding: t; -*-

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

;; Reuse OAuth tokens cached by the Codex CLI for gptel backends.
;; Treat the auth file like a password.

;;; Code:
(require 'cl-lib)
(require 'gptel)
(require 'json)
(require 'url)
(require 'url-util)

(defcustom gptel-codex-auth-file
  (expand-file-name "~/.codex/auth.json")
  "Path to Codex CLI cached auth.
Set cli_auth_credentials_store = \"file\" in ~/.codex/config.toml."
  :type 'file
  :group 'gptel)

(defcustom gptel-codex-issuer "https://auth.openai.com"
  "OAuth issuer used by Codex."
  :type 'string
  :group 'gptel)

(defcustom gptel-codex-client-id "app_EMoamEEZ73f0CkXaXp7hrann"
  "OAuth client_id used by the Codex CLI."
  :type 'string
  :group 'gptel)

(defcustom gptel-codex-expiry-skew 30
  "Seconds before expiry to refresh cached tokens."
  :type 'integer
  :group 'gptel)

(defcustom gptel-codex-access-token-ttl 3600
  "Fallback access-token lifetime in seconds when expiry is absent.
Used with the Codex CLI's last_refresh timestamp."
  :type '(choice
          (const :tag "Do not assume a TTL" nil)
          integer)
  :group 'gptel)

(defun gptel-codex--json-read-buffer ()
  (if (fboundp 'json-parse-buffer)
      (json-parse-buffer
       :object-type 'alist
       :array-type 'list
       :null-object nil
       :false-object :json-false)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string))
      (json-read))))

(defun gptel-codex--json-read-file (path)
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (gptel-codex--json-read-buffer))))

(defun gptel-codex--key-match-p (key candidate)
  (cond
   ((stringp candidate) (string= candidate key))
   ((symbolp candidate) (string= (symbol-name candidate) key))
   (t nil)))

(defun gptel-codex--alist-cell (key alist)
  (when (listp alist)
    (cl-loop for cell in alist
             when (and (consp cell)
                       (gptel-codex--key-match-p key (car cell)))
             return cell)))

(defun gptel-codex--alist-get* (keys alist)
  "Return first present key from KEYS (strings) in ALIST."
  (catch 'found
    (dolist (key keys)
      (let ((cell (gptel-codex--alist-cell key alist)))
        (when cell (throw 'found (cdr cell)))))
    nil))

(defun gptel-codex--now ()
  (float-time))

(defun gptel-codex--parse-time (time-string)
  "Parse TIME-STRING to epoch seconds or nil."
  (when (and (stringp time-string)
             (> (length time-string) 0))
    (ignore-errors
      (float-time (date-to-time time-string)))))

(defun gptel-codex--last-refresh (auth)
  (gptel-codex--alist-get* '("last_refresh" "lastRefresh") auth))

(defun gptel-codex--expires-at (auth)
  "Try common expiry fields; return epoch seconds or nil."
  (let ((val (gptel-codex--alist-get*
              '("expires_at" "expiresAt" "expiry" "expires" "access_token_expires_at")
              auth)))
    (cond
     ((numberp val) val)
     ((stringp val)
      (if (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" val)
          (string-to-number val)
        nil))
     (t nil))))

(defun gptel-codex--expiry-from-last-refresh (auth)
  "Infer expiry from last_refresh when explicit expiry is missing."
  (when-let* ((ttl gptel-codex-access-token-ttl)
              (last (gptel-codex--last-refresh auth))
              (parsed (gptel-codex--parse-time last)))
    (+ parsed ttl)))

(defun gptel-codex--access-token (auth)
  (gptel-codex--alist-get* '("access_token" "access" "token") auth))

(defun gptel-codex--refresh-token (auth)
  (gptel-codex--alist-get* '("refresh_token" "refresh") auth))

(defun gptel-codex--auth-alist-p (obj)
  (and (listp obj)
       (or (null obj)
           (and (consp (car obj))
                (or (stringp (caar obj))
                    (symbolp (caar obj)))))))

(defun gptel-codex--find-auth (obj)
  "Return the first alist that looks like an auth token container."
  (cond
   ((gptel-codex--auth-alist-p obj)
    (if (or (gptel-codex--access-token obj)
            (gptel-codex--refresh-token obj))
        obj
      (cl-loop for (_key . val) in obj
               for found = (gptel-codex--find-auth val)
               when found return found)))
   ((vectorp obj)
    (cl-loop for val across obj
             for found = (gptel-codex--find-auth val)
             when found return found))
   ((listp obj)
    (cl-loop for val in obj
             for found = (gptel-codex--find-auth val)
             when found return found))
   (t nil)))

(defun gptel-codex--alist-set (key value alist)
  "Set KEY to VALUE in ALIST, mutating ALIST in place when possible."
  (let ((cell (gptel-codex--alist-cell key alist)))
    (if cell
        (setcdr cell value)
      (let ((tail (last alist)))
        (if tail
            (setcdr tail (list (cons key value)))
          (setq alist (list (cons key value))))))
    alist))

(defun gptel-codex--http-post-form-json (url params)
  "POST x-www-form-urlencoded PARAMS to URL and parse JSON response."
  (let* ((normalized
          (cl-loop for pair in params
                   collect (if (consp pair)
                               (list (car pair) (cdr pair))
                             pair)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (url-build-query-string normalized))
         (buffer (url-retrieve-synchronously url t t 30)))
    (unless buffer
      (error "Failed to reach OAuth endpoint: %s" url))
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n" nil t)
            (let ((json (gptel-codex--json-read-buffer)))
              (when-let* ((err (alist-get "error" json nil nil #'string=)))
                (error "OAuth error: %s" err))
              json))
        (kill-buffer (current-buffer))))))

(defun gptel-codex--refresh (refresh-token)
  "Refresh access token using the OAuth token endpoint."
  (gptel-codex--http-post-form-json
   (concat gptel-codex-issuer "/oauth/token")
   `(("grant_type" . "refresh_token")
     ("refresh_token" . ,refresh-token)
     ("client_id" . ,gptel-codex-client-id))))

(defun gptel-codex--write-auth-file (auth)
  "Write AUTH back to `gptel-codex-auth-file` with 0600 perms."
  (let ((dir (file-name-directory gptel-codex-auth-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file gptel-codex-auth-file
    (insert (json-encode auth)))
  (ignore-errors (set-file-modes gptel-codex-auth-file #o600)))

(defun gptel-codex--needs-refresh-p (expires-at)
  (and (numberp expires-at)
       (> expires-at 0)
       (< (- expires-at gptel-codex-expiry-skew) (gptel-codex--now))))

(defun gptel-codex--ensure-valid-auth ()
  "Load auth from file and refresh if needed. Return updated alist."
  (let* ((raw (or (gptel-codex--json-read-file gptel-codex-auth-file)
                  (error "No readable Codex auth file: %s"
                         gptel-codex-auth-file)))
         (auth (or (gptel-codex--find-auth raw)
                   (error "Codex auth file missing token information")))
         (access (gptel-codex--access-token auth))
         (refresh (gptel-codex--refresh-token auth))
         (exp (or (gptel-codex--expires-at auth)
                  (gptel-codex--expiry-from-last-refresh raw))))
    (unless access
      (error "Codex auth file missing access token fields"))
    (when (gptel-codex--needs-refresh-p exp)
      (unless refresh
        (error "Access token expired/near-expiry and no refresh token found"))
      (let* ((tok (gptel-codex--refresh refresh))
             (new-access (gptel-codex--alist-get*
                          '("access_token" "access" "token") tok))
             (new-refresh (or (gptel-codex--alist-get*
                               '("refresh_token" "refresh") tok)
                              refresh))
             (expires-in (gptel-codex--alist-get*
                          '("expires_in" "expiresIn") tok))
             (expires-secs (cond
                            ((numberp expires-in) expires-in)
                            ((stringp expires-in) (string-to-number expires-in))
                            (t 3600)))
             (new-exp (+ (gptel-codex--now) expires-secs)))
        (unless new-access
          (error "OAuth refresh response missing access token"))
        (setq auth (gptel-codex--alist-set "access_token" new-access auth))
        (setq auth (gptel-codex--alist-set "refresh_token" new-refresh auth))
        (setq auth (gptel-codex--alist-set "expires_at" new-exp auth))
        (gptel-codex--write-auth-file raw)))
    auth))

;;;###autoload
(defun gptel-codex-auth-header ()
  "Header function for gptel :header. Injects Bearer token."
  (let* ((auth (gptel-codex--ensure-valid-auth))
         (access (gptel-codex--access-token auth)))
    `(("Authorization" . ,(concat "Bearer " access)))))

(provide 'gptel-codex-auth)
;;; gptel-codex-auth.el ends here
