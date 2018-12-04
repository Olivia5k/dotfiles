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

(defmacro th/mode-menu--body (mode name)
  `(let ((buffers
         (--map
          (buffer-name it)
          (--filter
           (equal ',mode (with-current-buffer it major-mode))
           (buffer-list)))))

    (cond
     ((= (length buffers) 1)
      (switch-to-buffer (car buffers))
      (message "Visiting only open %s buffer" ,name))

     ((> (length buffers) 1)
      (switch-to-buffer
       (completing-read (format "%s buffers: " ,name) buffers)))

     (t
      (message "There are no %s buffers open right now" ,name)))))

(defmacro th/mode-menu (mode)
  (let* ((name (symbol-name mode))
         (defun-name (make-symbol (format "%s-select-buffer" name))))

    `(defun ,defun-name ()
       (interactive)
       (th/mode-menu--body ,mode ,name))))

(defun th/mode-buffers ()
  (interactive)
  (let* ((modes
         (-sort 'string<
                (-uniq (--map
                        (with-current-buffer it major-mode)
                        (buffer-list)))))
         (mode (completing-read "major-mode: " modes nil t)))
    (th/mode-menu--body (make-symbol mode) mode)))

(global-set-key (kbd "C-x M-d") (th/mode-menu dired-mode))
(global-set-key (kbd "C-x M-c") (th/mode-menu compilation-mode))
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

(defun th/buffer-or-back (name)
  "Switch to the named buffer, or go back if we are already are visiting it."
  (interactive)
  (switch-to-buffer
   (if (string-equal (buffer-name) name)
       (other-buffer (current-buffer) 1)
     name)))

(global-set-key (kbd "s-c") (lambda () (interactive) (th/buffer-or-back "*compilation*")))

(provide 'th-toggle)
