(require 'eshell)
;; (require 'em-tramp)
(setq eshell-prefer-lisp-functions nil)
(setq eshell-prefer-lisp-variables nil)
(setq password-cache nil) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil
      eshell-destroy-buffer-when-process-dies t)

(use-package eshell-prompt-extras
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(defun th/eshell-here (arg)
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive "P")
  (let* ((root-buffer (window-buffer (selected-window)))
         (parent (if (buffer-file-name root-buffer)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (shellname (concat "*eshell: " parent "*"))
         (shell-buffer (get-buffer shellname)))

    (when (not arg)
      (split-window-below -20)
      (windmove-down))

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

(defun th/eshell-dired ()
  (interactive)
  (th/eshell-here 1))

(global-set-key (kbd "C-x e") #'th/eshell-here)

(setenv "PAGER" "cat")
(defalias 'e 'find-file)

(global-set-key (kbd "C-x M-e") (th/mode-menu eshell-mode))

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

(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (re-search-forward eshell-prompt-regexp nil t n)
  (when eshell-highlight-prompt
    (while (not (get-text-property (line-beginning-position) 'read-only) )
      (re-search-forward eshell-prompt-regexp nil t n)))
  (eshell-skip-prompt))

(defun eshell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (backward-char)
  (eshell-next-prompt (- n)))

(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  (interactive)
  (insert (ivy-completing-read "Eshell history: "
                               (delete-dups
                                (ring-elements eshell-history-ring)))))

(add-hook
 'eshell-mode-hook
 (lambda ()
   (define-key eshell-mode-map (kbd "M-P") 'eshell-previous-prompt)
   (define-key eshell-mode-map (kbd "M-N") 'eshell-next-prompt)
   (define-key eshell-mode-map (kbd "M-r") 'eshell-insert-history)
   (define-key eshell-mode-map (kbd "M-s") 'th/eshell-toggle-sudo)))

(provide 'th-eshell)
