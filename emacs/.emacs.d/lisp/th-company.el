(use-package company
  :config
  (use-package company-flx
    :config
    (company-flx-mode +1))

  ;; Let company do its thing as often as possible.
  (global-company-mode t)

  (setq company-tooltip-limit 20)       ; bigger popup window
  (setq company-idle-delay .3) ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)  ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  )

(use-package company-go
  :after company)

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

(provide 'th-company)
