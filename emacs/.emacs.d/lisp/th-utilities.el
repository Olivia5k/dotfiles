;; Major mode utilities - little to no config
(use-package crontab-mode
  :mode ("\\.cron\\'" . crontab-mode))
(use-package csv-mode)
(use-package editorconfig
  :init (editorconfig-mode 1))
(use-package fish-mode)
(use-package highlight-symbol
  :config (setq highlight-symbol-idle-delay 0))
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
(use-package fill-column-indicator
  :config (setq fci-rule-color "#373b41"))
(use-package paradox
  :config (setq paradox-github-token t))
(use-package paredit)
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))
(use-package counsel-spotify)
(use-package sqlup-mode
  :config (add-hook 'sql-mode-hook 'sqlup-mode))
(use-package transpose-frame)
(use-package wrap-region)
(use-package xkcd)



;; builtins
(use-package semantic
  :disabled t
  :ensure nil
  :config
  (setq global-semantic-idle-scheduler-mode nil)
  (setq global-semanticdb-minor-mode nil))
(use-package sh-script
  :ensure nil
  :config
  (setq sh-indentation 4))
(use-package vc
  :ensure nil
  :config (setq vc-follow-symlinks t))
(use-package edit-server
  :demand t
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(provide 'th-utilities)
