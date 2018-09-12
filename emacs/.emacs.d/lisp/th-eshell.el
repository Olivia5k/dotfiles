(require 'eshell)
;; (require 'em-tramp)
(setq eshell-prefer-lisp-functions nil)
(setq eshell-prefer-lisp-variables nil)
(setq password-cache nil) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

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
  (let* ((root-buffer (window-buffer (selected-window)))
         (parent (if (buffer-file-name root-buffer)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (shellname (concat "*eshell: " parent "*"))
         (shell-buffer (get-buffer shellname)))

    (if shell-buffer
        ;; If we're already in eshell, go back to where we were.
        ;; Otherwise, switch to it said eshell.
        (if (string= (buffer-name root-buffer)
                     (buffer-name shell-buffer))
            (previous-buffer)
          (switch-to-buffer shell-buffer))
      ;; It doesn't exist yet - let's create it!
      (eshell "new")
      (rename-buffer shellname))))

(global-set-key (kbd "C-x e") #'th/eshell-here)

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

(defun th/eshell-toggle-sudo ()
  "Add sudo at the beginning of the current line.

If already there, remove it."
  (interactive)
  (save-excursion
    (eshell-bol)
    (if (looking-at "sudo")
        (delete-forward-char 5)
      (insert "sudo "))))

(bind-key "C-c C-s" #'th/eshell-toggle-sudo eshell-mode-map)

(provide 'th-eshell)
