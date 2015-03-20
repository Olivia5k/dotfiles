(global-set-key (kbd "M-g") 'magit-status)

(setq magit-save-some-buffers 'dontask
      )

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  "Restores the previous window configuration and kills the magit buffer"
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-mode-quit-window)

(provide 'local-magit)
