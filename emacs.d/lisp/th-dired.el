(use-package dired
  :ensure nil
  :demand
  :bind (:map dired-mode-map
              ("M-r" . rgrep))

  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (setq-default dired-omit-files-p t)   ; Buffer-local variable

  (setq dired-hide-details-hide-information-lines t)
  (setq dired-hide-details-mode t)
  (setq diredp-hide-details-initially-flag t)
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (put 'dired-find-alternate-file 'disabled nil)

  (use-package dired-x
    :ensure nil
    :demand t
    :config
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|.pyc$\\|.sock$"))
    (setq dired-omit-verbose nil) ;; https://open.spotify.com/track/2XRl0NfORYPEvUJXLtJiND
    (setq dired-omit-mode t))

  (use-package dired-subtree
    :after (dired)))

(provide 'th-dired)
