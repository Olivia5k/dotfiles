(defvar exwm-status-hooks
  '(exwm-status-date
    exwm-status-week
    exwm-status-unixtime
    exwm-status-utc
    exwm-status-battery))

(defvar exwm-status-always-on '(exwm-status-date exwm-status-unixtime))

(defun exwm-status-date ()
  "Returns the date"
  (format-time-string "%Y-%m-%d %T"))

(defun exwm-status-week ()
  "Returns the week information"
  (format-time-string "@ %a w%W"))

(defun exwm-status-unixtime ()
  "Returns the seconds since the epoch"
  (format-time-string "[%s]"))

(defun exwm-status-utc ()
  "Returns the time in UTC"
  (format-time-string "(%T UTC)" nil t))

(defun exwm-status-battery ()
  "Returns battery status, joining multiple batteries together"
  (s-replace
   "\n" "; "
   (s-trim (shell-command-to-string "acpi -b"))))

(defun exwm-status-battery-p ()
  "Prejudicate to determine if we're running the battery hook or not"
  (executable-find "acpi"))

(defun exwm-status-exec (hook force-run)
  "Runs a single hook"
  (let* ((name (symbol-name hook))
         (pred (intern (format "%s-p" name))))

    ;; If we are forcing all the hooks to run, or we've defined the current
    ;; hook to be always on, we should execute it.
    (when (or force-run (memq hook exwm-status-always-on))
      ;; If no prejudicate is set for the current hook - run it.
      ;; Otherwise, run the prejudicate to see if we should execute the hook.
      (when (or (not (fboundp pred))
                (funcall pred))
        (exwm-status-escape (funcall hook))))))

;;
(defun exwm-status-escape (s)
  "Escapes a string"
  ;; Replace percent signs with a double percent since `message' will be upset otherwise.
  (s-replace "%" "%%" s))

(defun exwm-status (arg)
  (interactive "P")
  (message (s-join " " (remove-if nil (mapcar (lambda (f) (funcall 'exwm-status-exec f arg))
                                              exwm-status-hooks)))))

(exwm-input-set-key (kbd "s-.") #'exwm-status)

(provide 'exwm-status)
