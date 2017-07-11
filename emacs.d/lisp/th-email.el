(use-package mu4e
  :bind ("C-x m" . mu4e)
  :demand
  :ensure nil

  :init
  (setq mu4e-maildir "~/.mail/")
  (setq mu4e-headers-date-format "%Y-%m-%d")
  (setq mu4e-completing-read-function 'ivy-completing-read)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-confirm-quit nil))

(provide 'th-email)
