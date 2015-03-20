;;; minimal configuration file for emacs as system default editor

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'packages)
(require 'options)
(require 'interface)
(require 'magit)
(require 'pytest)
(require 'window-management)

(global-set-key (kbd "C-c C-e") 'eval-buffer)
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)


(setq debug-on-error t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq ido-create-new-buffer 'always
      ido-default-buffer-method 'selected-window
      ido-case-fold t
      ido-enable-last-directory-history nil
      ido-use-filename-at-point nil
      ido-use-url-at-pointt nil
      ido-enable-flex-matching t
      ido-max-prospects 15
      ido-confirm-unique-completion t
      ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                        " [No match]" " [Matched]" " [Not readable]"
                        " [Too big]" " [Confirm]"))

(require 'ido)
(ido-mode t)
(ido-everywhere t)

;; remove .elc when saving .el
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'after-save-hook)
            (add-hook 'after-save-hook
                      (lambda ()
                        (if (file-exists-p (concat buffer-file-name "c"))
                            (delete-file (concat buffer-file-name "c")))))))

(define-key global-map (kbd "M-g") 'goto-line)

(define-key global-map (kbd "C-<f5>")
  (lambda ()
    (interactive)
    (let ((content initial-scratch-message)
          (buf "*scratch*"))
      (when (get-buffer buf)
        (setq content ""))
      (switch-to-buffer buf)
      (insert content))))

(define-key global-map (kbd "C-x M-f")
  (lambda (&optional arg)
    (interactive "p")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/su::" (ido-read-file-name "File (Tramp): " "/")))
        (find-alternate-file (concat "/su::" buffer-file-name)))))


(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 2               ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 20            ; number of keystrokes between auto-saves (default: 300)
      )

;; window switching
(define-key global-map (kbd "C-<tab>")
  (lambda () (interactive) (select-window (next-window))))
(define-key global-map (kbd "C-S-<iso-lefttab>")
  (lambda () (interactive) (select-window (previous-window))))

;; window resizing
(define-key global-map (kbd "S-C-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-C-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "S-C-<down>") 'shrink-window)
(define-key global-map (kbd "S-C-<up>") 'enlarge-window)

(define-key global-map (kbd "M-0") 'delete-window)
(define-key global-map (kbd "M-1") 'delete-other-windows)
(define-key global-map (kbd "M-2") 'split-window-vertically)
(define-key global-map (kbd "M-3") 'split-window-horizontally)
(define-key global-map (kbd "M-4") 'kill-buffer-and-window)
(define-key global-map (kbd "M-=") 'balance-windows)




;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(set-default-font "Inconsolata-16")
(load-theme 'zenburn t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (discover-my-major yasnippet yaml-mode ssh-config-mode smex smart-mode-line rainbow-mode rainbow-identifiers rainbow-delimiters python-environment puppet-mode projectile paredit paradox otp nginx-mode morlock markdown-mode magit js2-mode ioccur ido-vertical-mode ido-ubiquitous httpcode htmlize gitignore-mode gitconfig-mode gist fringe-helper flycheck flx-ido fill-column-indicator expand-region epc dockerfile-mode diminish csv-mode crontab-mode buffer-move browse-kill-ring auto-complete ack-and-a-half ace-jump-mode ac-dabbrev))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
