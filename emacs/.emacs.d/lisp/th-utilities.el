;; Major mode utilities - little to no config
(use-package crontab-mode
  :mode ("\\.cron\\'" . crontab-mode))
(use-package csv-mode)
(use-package editorconfig
  :diminish editorconfig-mode
  :init (editorconfig-mode 1))
(use-package fish-mode)
(use-package highlight-symbol
  :config (setq highlight-symbol-idle-delay 0))
(use-package logview)
(use-package markdown-mode
  :config (setq markdown-asymmetric-header t))
(use-package nginx-mode)
(use-package pass
  :config
  ;; Remember to set ~/.gnupg/gpg-agent.conf to have `pinentry-program /usr/bin/pinentry-emacs`
  (setq epa-pinentry-mode 'loopback))
(use-package ivy-pass)

(use-package protobuf-mode)
(use-package puppet-mode)
(use-package ssh-config-mode)
(use-package yaml-mode)
(use-package systemd)
(use-package daemons)


;; minor mode utilities - little to no config
(use-package adaptive-wrap)
(use-package buffer-move)
(use-package define-word)
(use-package fill-column-indicator
  :config (setq fci-rule-color "#373b41"))
(use-package paradox
  :config (setq paradox-github-token t))
(use-package paredit)
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))
(use-package counsel-spotify)
(use-package sql
  :ensure nil
  :init (add-to-list 'same-window-buffer-names "*SQL*"))
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
  (global-semantic-idle-scheduler-mode nil)
  (global-semanticdb-minor-mode nil))
(use-package sh-script
  :ensure nil
  :config
  (setq sh-indentation 4))
(use-package vc
  :ensure nil
  :config (setq vc-follow-symlinks t))
(use-package edit-server
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))
(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh"))


;; Simple functions that have nowhere else to live
(defun th/unixtime (arg)
  "Prints a date of the unixtime under point.

If given the universal argument, the current unixtime is inserted
at point."
  (interactive "p")

  (if (= arg 4)
      (insert (s-trim (shell-command-to-string "date +%s")))
    (let* ((thing (substring-no-properties (symbol-name (symbol-at-point)))))
      (shell-command (format "date -d @%s" thing)))))

(global-set-key (kbd "C-x t") 'th/unixtime)

(provide 'th-utilities)
