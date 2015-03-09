(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini) ;; I do this way too much...

(require 'projectile)
(projectile-global-mode)

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-z") 'undo)

(require 'fill-column-indicator)
(fci-mode 1)
(setq fci-rule-width 1)
(setq fci-rule-color "#444444")
(add-hook 'after-change-major-mode-hook 'fci-mode)


(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'emacs-lisp-mode-hook 'rainbow-identifiers-mode)

(provide 'interface)
