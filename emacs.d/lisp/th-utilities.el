;; Major mode utilities - little to no config
(use-package crontab-mode)
(use-package csv-mode)
(use-package editorconfig
  :init (editorconfig-mode 1))
(use-package fish-mode)
(use-package logview)
(use-package markdown-mode
  :config (setq markdown-asymmetric-header t))
(use-package nginx-mode)
(use-package pass)
(use-package protobuf-mode)
(use-package puppet-mode)
(use-package ssh-config-mode)
(use-package yaml-mode)


;; minor mode utilities - little to no config
(use-package adaptive-wrap)
(use-package buffer-move)
(use-package diminish)
(use-package fill-column-indicator)
(use-package paradox)
(use-package paredit)
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))
(use-package sqlup-mode
  :config (add-hook 'sql-mode-hook 'sqlup-mode))
(use-package transpose-frame)
(use-package wrap-region)
(use-package xkcd)


(provide 'th-utilities)
