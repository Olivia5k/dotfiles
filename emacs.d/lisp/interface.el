(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x v") 'ido-switch-buffer) ;; I do this way too much...

(require 'projectile)
(projectile-global-mode)

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-z") 'undo)

(require 'fill-column-indicator)
(fci-mode 1)
(setq fci-rule-width 1)
(setq fci-rule-color "#444444")


(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(provide 'interface)
