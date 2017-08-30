;;; ssh-agent.el --- Using ssh-agent and ssh-add from Emacs.

;; Copyright (C) 2014  Daisuke Kobayashi

;; Author: Daisuke Kobayashi <d5884jp@gmail.com>
;; Version: 0.1
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supports to use `ssh-agent' from Emacs at ease.
;; If you want to use ssh or scp with ssh-agent from Emacs, call `ssh-agent-add-key'.
;;  * `ssh-agent' will be automatically invoked if it isn't run yet,
;;    and be killed when Emacs is exited.
;;  * If the private key is already regisitered, do nothing, so you can call
;;    simply on every point it is likely to be required.

;;; Example:

;; * Using with TRAMP:
;;   (defadvice tramp-send-command (before ssh-agent-with-tramp activate)
;;    (ssh-agent-add-key))

;; * Using with Magit:
;;   (defadvice magit-push-dwim (before ssh-agent-with-magit-push activate)
;;    (ssh-agent-add-key))
;;   (defadvice magit-fetch (before ssh-agent-with-magit-fetch activate)
;;    (ssh-agent-add-key))

;; NOTE(thiderman): This is a full download from https://github.com/d5884/ssh-agent/
;;                  but was added here in its entirety since it was
;;                  not made available as an installable module.

;;; Code:

(defgroup ssh-agent nil "Using ssh-agent and ssh-add from Emacs."
  :group 'processes)

(defcustom ssh-agent-program (executable-find "ssh-agent")
  "Path for `ssh-agent' executable."
  :group 'ssh-agent
  :type 'file)

(defcustom ssh-agent-add-program (executable-find "ssh-add")
  "Path for `ssh-add' executable."
  :group 'ssh-agent
  :type 'file)

(defcustom ssh-agent-kill-on-exit t
  "If this value is non-nil, the `ssh-agent' invoked by the Emacs will be kill when `kill-emacs' is called."
  :group 'ssh-agent
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

(defcustom ssh-agent-run-on-demand t
  "If non-nil, `ssh-agent-add-key' calls `ssh-agent-run' if `ssh-agent' isn't invoked yet."
  :group 'ssh-agent
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

(defcustom ssh-agent-use-env-file t
  "If this value is non-nil, `ssh-agent-run' will use `ssh-agent-env-file'.
Set non-nil if you need to access `ssh-agent' invoked by Emacs/other process from
 other process/Emacs.
See `ssh-agent-env-file'."
  :group 'ssh-agent
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

(defcustom ssh-agent-env-file (expand-file-name "ssh-agent-env" "~/.ssh")
  "File name for ssh-agent process information file.
`ssh-agent-run' treats this file as an output of `ssh-agent -s' or
 `ssh-agent -c'.
If you want to access invoked by Emacs from other process,
load this file on other process like \". ~/.ssh/ssh-agent-env\".
If you want to access invoked by other process from Emacs,
write this file on other process like \"ssh-agent -s > ~/.ssh/ssh-agent-env\"."
  :group 'ssh-agent
  :type 'file)

(defcustom ssh-agent-env-file-type (cond ((string-match-p "csh" shell-file-name)
					  'csh)
					 (t
					  'sh))
  "Type of `ssh-agent-env-file'.
This accepts below:
sh - file is bourne shell script (sh/bash/ksh/zsh).
 An output of `ssh-agent -s'
csh - file is c-shell script (csh/tcsh).
 An output of `ssh-agent -c'
A default value is determined by `shell-file-name'."
  :group 'ssh-agent
  :type '(choice (const :tag "C-shell type (csh/tcsh)" csh)
		 (const :tag "Bourne shell type (sh/bash/ksh/zsh)" sh)))

(defcustom ssh-agent-env-file-coding-system nil
  "Coding system for `ssh-agent-env-file'."
  :group 'ssh-agent
  :type 'coding-system)

(defvar ssh-agent-env-file-mode #o600
  "File mode for `ssh-agent-env-file'.")

(defvar ssh-agent-env-pattern-alist
  '((sh  . "^\\([a-zA-Z_][a-zA-Z_0-9]*\\)=\\([^;]+\\); export \\1;")
    (csh . "^setenv \\([a-zA-Z_][a-zA-Z_0-9]*\\) \\([^;]+\\);"))
  "Regexp pattern for ssh-agent envrionment output by shell type.")

(defvar ssh-agent-env-file-made-by-emacs nil
  "If non-nil, `ssh-agent-env-file' is made by the Emacs.")

(defvar ssh-agent-add-prompt-regexp
  "^\\(Enter passphrase\\|Bad passphrase, try again\\) for [^:]*:"
  "Regexp for `ssh-add' passphrase prompt.")

;;;###autoload
(defun ssh-agent-add-key (&optional key-file)
  "Add KEY-FILE into `ssh-agent'.
If KEY-FILE is alreadly registered, do nothing.
If KEY-FILE is nil, use default key.
If `ssh-agent-run-on-demand' is non-nil, run `ssh-agent-run' as necessary."
  (interactive)
  (if (not (or (ssh-agent-live-p)
	       (and ssh-agent-run-on-demand (ssh-agent-run))))
      (message "Could not open a connection to your authentication agent.")
    (or (ssh-agent-registered-p key-file)
	(let* ((process-connection-type 'pty)
	       (process (start-process "*ssh-add*" nil ssh-agent-add-program)))
	  (set-process-query-on-exit-flag process nil)
	  (set-process-filter process
			      (lambda (proc msg)
				(save-match-data
				  (cond
				   ((string-match ssh-agent-add-prompt-regexp msg)
				    (let ((password (read-passwd
						     (replace-regexp-in-string
						      "\\`[\r\n\s]*" "" msg))))
				      (send-string proc password)
				      (clear-string password)
				      (send-string proc "\n")))
				   (t
				    (message "%s" (replace-regexp-in-string
						   "\\`[\r\n\s]*\\|[\s\r\n]*\\'" "" msg))))
				  )))
	  (while (process-live-p process)
	    (sleep-for 0 1))
	  (eq 0 (process-exit-status process))))))

;;;###autoload
(defun ssh-agent-delete-key (&optional key-file)
  "Delete KEY-FILE from `ssh-agent'.
If KEY-FILE is nil, delete default key.
If KEY-FILE is non-nil without string, delete all key."
  (interactive)
  (eq 0 (apply 'call-process
	       ssh-agent-add-program nil nil nil
	       (cond
		((null key-file)
		 '("-d"))
		((stringp key-file)
		 `("-d" ,key-file))
		(t
		 '("-D"))))))

;;;###autoload
(defun ssh-agent-run ()
  "Run `ssh-agent' as necessary.
If environment variable `SSH_AGENT_PID' and `SSH_AUTH_SOCK' is set,
do nothing."
  (interactive)
  (when ssh-agent-use-env-file
    (ssh-agent--read-env-from-file))

  (or (ssh-agent-live-p)
      (let ((result (with-temp-buffer
		      (when (eq 0 (call-process
				   ssh-agent-program nil (current-buffer) nil
				   (if (eq ssh-agent-env-file-type 'sh)
				       "-s" "-c")))
			(buffer-string)))))
	(when result
	  (ssh-agent--parse-env-string result)
	  (when ssh-agent-use-env-file
	    (ssh-agent--write-env-file result))
	  (when ssh-agent-kill-on-exit
	    (add-hook 'kill-emacs-hook 'ssh-agent-kill))
	  t))))

;;;###autoload
(defun ssh-agent-kill ()
  "Kill `ssh-agent' process."
  (interactive)
  (when (and (ssh-agent-live-p)
	     (eq 0 (call-process ssh-agent-program nil nil nil
				 "-k")))
    (setenv "SSH_AGENT_PID" nil)
    (setenv "SSH_AUTH_SOCK" nil)
    (when ssh-agent-env-file-made-by-emacs
      (delete-file ssh-agent-env-file))
    ))

;;;###autoload
(defun ssh-agent-live-p ()
  "Non-nil when `ssh-agent' is live and accessible."
  (and
   (getenv "SSH_AGENT_PID")
   (getenv "SSH_AUTH_SOCK")
   (eq 0 (signal-process (string-to-number (getenv "SSH_AGENT_PID")) 0))))

;;;###autoload
(defun ssh-agent-registered-p (&optional key-file)
  "Non-nil when KEY-FILE is registered on `ssh-agent'.
If key-file is nil, return t when any key is registered."
  (let ((result (with-temp-buffer
		  (when (eq 0 (call-process
			       ssh-agent-add-program nil (current-buffer) nil
			       "-L"))
		    (buffer-string)))))
    (and
     result
     (or (null key-file)
	 (string-match-p (concat "\s" (regexp-quote key-file) "$") result)))))

(defun ssh-agent--read-env-from-file ()
  "Read environment variables for `ssh-agent' from `ssh-agent-env-file'."
  (with-temp-buffer
    (when (and (file-exists-p ssh-agent-env-file)
	       (file-readable-p ssh-agent-env-file)
	       (ignore-errors
		 (let ((coding-system-for-read ssh-agent-env-file-coding-system))
		   (insert-file-contents ssh-agent-env-file))))
      (setq ssh-agent-env-file-made-by-emacs nil)
      (ssh-agent--parse-env-string (buffer-string)))))

(defun ssh-agent--write-env-file (env-string)
  "Write ENV-STRING into `ssh-agent-env-file'."
  (with-temp-buffer
    (insert env-string)
    (let ((coding-system-for-write ssh-agent-env-file-coding-system))
      (write-region (point-min) (point-max) ssh-agent-env-file nil 'silent))
    (set-file-modes ssh-agent-env-file ssh-agent-env-file-mode)
    (setq ssh-agent-env-file-made-by-emacs t)))

(defun ssh-agent--parse-env-string (env-string)
  "Parse ENV-STRING as output of `ssh-agent -s' or `ssh-agent -c'."
  (let ((pos 0))
    (save-match-data
      (while (string-match (cdr (assq ssh-agent-env-file-type
				      ssh-agent-env-pattern-alist)) env-string pos)
	(let ((name (match-string 1 env-string))
	      (value (match-string 2 env-string)))
	  (setq pos (match-end 0))     ; `setenv' destroys match-data.
	  (setenv name value)))
      )))

(provide 'th-ssh)
