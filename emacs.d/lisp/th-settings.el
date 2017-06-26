;; This uses the `chrome-app` command from the dotfiles repo and puts browser
;; configuration outside of emacs.
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "chrome-app")
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; *scratch*\n\n")
(setq scroll-step 10)
(setq debug-on-error nil)
(setq gc-cons-threshold 50000000)

(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines nil)

(provide 'th-settings)
