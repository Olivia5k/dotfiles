;; Major mode utilities - little to no config
(use-package crontab-mode
  :mode ("\\.cron\\'" . crontab-mode))
(use-package csv-mode)
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
(use-package ssh-config-mode)
(use-package terraform-mode)
(use-package yaml-mode)
(use-package systemd)
(use-package daemons)


;; minor mode utilities - little to no config
(use-package adaptive-wrap)
(use-package buffer-move)
(use-package define-word)
(use-package fill-column-indicator
  :config
  (setq fci-rule-color "#4b2d54")
  (setq-default fill-column 100))
(use-package paredit
  :diminish paredit-mode)
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))
(use-package counsel-spotify)
(use-package sqlup-mode
  :hook ((sql-mode-hook . sqlup-mode)
         (sql-mode-hook . yas-minor-mode-on)))
(use-package transpose-frame)
(use-package wrap-region)
(use-package xkcd)


;; builtins
(require 'semantic)
(setq-default global-semantic-idle-scheduler-mode nil)
(setq-default global-semanticdb-minor-mode nil)
(semantic-mode -1)
(setq sh-indentation 4)
(setq vc-follow-symlinks t)

(use-package edit-server
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(setq tramp-default-method "ssh")


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
