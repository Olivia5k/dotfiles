;; This uses the `chrome-app` command from the dotfiles repo and puts browser
;; configuration outside of emacs.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chrome-app")



(fset 'yes-or-no-p 'y-or-n-p)


(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

(setq scroll-step 10)
(setq debug-on-error nil)

(set-default 'truncate-lines nil)

(setq gc-cons-threshold 50000000)

(provide 'th-settings)
