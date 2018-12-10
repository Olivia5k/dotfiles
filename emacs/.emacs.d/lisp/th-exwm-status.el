(defvar th/status-hooks '(th/es/date th/es/time-utc))
(setq th/status-hooks '(th/es/date th/es/time-utc))

(defun th/es/date ()
  "Returns the date"
  (format-time-string "%Y-%m-%d %T @ %a w%W [%s]"))

(defun th/es/time-utc ()
  "Returns the time in UTC"
  (format-time-string "(%T UTC)" nil t))

(defun th/es/battery ()
  "Returns battery status, joining multiple batteries together"
  (s-replace
   "%" "%%"
   (s-replace
    "\n" "; "
    (s-trim (shell-command-to-string "acpi -b")))))

(defun th/exwm-status ()
  (interactive)
  (message (s-join " " (mapcar 'funcall th/status-hooks))))

(exwm-input-set-key (kbd "s-.") #'th/exwm-status)

;; Only add the battery on the laptop
(when (string-equal (system-name) "dragonwing")
  (setq th/status-hooks
        (append th/status-hooks '(th/es/battery))))

(provide 'th-exwm-status)
