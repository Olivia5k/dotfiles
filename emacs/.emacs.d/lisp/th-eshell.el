(use-package eshell-prompt-extras
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(defun th/eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t))))
         (shellname (concat "*eshell: " name "*")))

    (if (not (get-buffer shellname))
        ;;
        (progn
          (eshell "new")
          (rename-buffer shellname))
      ;; If we're already in eshell, go back to where we were.
      ;; Otherwise, switch to it said eshell.
      (if (derived-mode-p 'eshell-mode)
          (previous-buffer)
        (switch-to-buffer shellname)))))

(global-set-key (kbd "s-<return>") #'th/eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(setenv "PAGER" "cat")
(defun th/eshell-menu ()
  "Go to one of the currently open eshell buffers (if there is one)."
  (interactive)
  (let* ((eshell-buffers (--map (buffer-name it)
                                (--filter
                                 (equal 'eshell-mode (with-current-buffer it major-mode))
                                 (buffer-list)))))
    (if eshell-buffers
        (switch-to-buffer (completing-read "Select eshell: " eshell-buffers))
      (message "There are no eshell buffers open right now"))))

(defalias 'e 'find-file)

(global-set-key (kbd "C-x M-e") 'th/eshell-menu)

(provide 'th-eshell)
