;; Libraries that are simply useful always
(use-package no-littering)
(use-package cl-lib)
(use-package dash)
(use-package f)
(use-package s)
(use-package hydra)
(use-package ivy)
(use-package autothemer)
(use-package tramp)

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-function)
  (diminish 'auto-revert-mode)
  (diminish 'compilation-in-progress)
  (diminish 'eldoc-mode)
  (diminish 'projectile-mode))

;; This uses the `browser-app` command from the dotfiles repo and puts browser
;; configuration outside of emacs.
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "browser")
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; *scratch*\n\n")
(setq scroll-step 10)
(setq scroll-conservatively 100)
(setq debug-on-error nil)
(setq gc-cons-threshold 50000000)
(setq-default bidi-display-reordering nil)


(fset 'yes-or-no-p 'y-or-n-p)

;; I always thought this was the other way around :'(
(set-default 'truncate-lines t)

(setenv "GOPATH" "$HOME" t)

(setq th/custom-paths
      (-map 'substitute-env-vars
            '("$HOME/.local/bin"
              "$HOME/.local/share/infect/util"
              "$GOPATH/bin")))

;; Set new PATH elements that won't be there otherwise
(unless (s-contains? ".local/" (getenv "PATH"))
  (setenv "PATH"
          (concat
           (s-join ":" th/custom-paths)
           ":"
           (getenv "PATH")))
  (setq exec-path (append th/custom-paths exec-path)))


(provide 'th-core)
