;; This uses the `chrome-app` command from the dotfiles repo and puts browser
;; configuration outside of emacs.
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "chrome-app")
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; *scratch*\n\n")
(setq scroll-step 0)
(setq debug-on-error nil)
(setq gc-cons-threshold 50000000)

(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines nil)

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

(provide 'th-settings)
