(defun th/toggle-buffer (func name &optional kill-window)
  "Toggle or destroy a buffer, depending on if it exists or not.

The `func` argument should be a callable that toggles the buffer.
The `name` argument is a substring of the buffer that should be matched."
  (interactive)
  (let ((done nil))
    (loop for buffer being the buffers
          do (let ((bname (buffer-name buffer)))
               (when (s-contains? name bname)
                 (if kill-window
                     (progn
                       (select-window (get-buffer-window buffer))
                       (kill-buffer-and-window)
                       (message "Killed %s" bname))
                   (progn
                     (kill-buffer buffer)
                     (message "%s toggled away" bname)))
                 (setq done t))))
    (unless done
      (funcall func))))

(defun th/toggle-file (path &optional no-save)
  (let ((file (file-truename path)))
    (if (s-equals? file buffer-file-name)
        (progn
          (when (not no-save)
            (save-buffer))
          (previous-buffer))
      (find-file file))))

(defun th/kill-file-name (p)
  "Adds the current file name to the kill ring, with the current project prefix removed.

Adding universal argument will display the full path regardless."

  (interactive "P")
  (let ((fn (file-truename (buffer-name))))
    (kill-new
     (message
      (if (and (projectile-project-p)
               (not p))
          (s-chop-prefix (projectile-project-root) fn)
        fn)))))

(define-key global-map (kbd "C-x ?") 'th/kill-file-name)


(defun th/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (cond
   ((string-equal (buffer-name) "Spotify")
    (switch-to-buffer "Chromium"))
   ((string-equal (buffer-name) "Chromium")
    (if (get-buffer "Spotify")
        (switch-to-buffer "Spotify")
      (exwm-execute "spotify")))
   (t
    (switch-to-buffer (other-buffer (current-buffer) 1)))))

(global-set-key (kbd "C-x C-b") 'th/switch-to-previous-buffer)


(defun th/dired-menu ()
  "Go to one of the currently open dired buffers (if there is one)."
  (interactive)
  (let* ((dired-buffers (--map (buffer-name it)
                               (--filter
                                (equal 'dired-mode (with-current-buffer it major-mode))
                                (buffer-list)))))
    (if dired-buffers
        (switch-to-buffer (completing-read "Select dired: " dired-buffers))
      (message "There's no dired buffers open right now"))))

(global-set-key (kbd "C-x M-d") 'th/dired-menu)
(global-set-key (kbd "C-x M-b") 'ibuffer)

(defun th/goto-emacs-file-split ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (windmove-down)
    (th/toggle-file (concat user-emacs-directory "init.el")))

(define-key global-map (kbd "C-x <f2>") #'th/goto-emacs-file-split)

(defun th/goto-scratch ()
    (interactive)
    (let ((content initial-scratch-message)
          (buf "*scratch*"))
      (when (get-buffer buf)
        (setq content ""))
      (switch-to-buffer buf)
      (insert content)))

(define-key global-map (kbd "<f4>") #'th/goto-scratch)

(defun th/toggle-debug ()
    (interactive)
    (let ((doe t))
      (if debug-on-error
          (setq doe nil))
      (setq debug-on-error doe)
      (message "debug-on-error set to %s" doe)))

(provide 'th-toggle)
