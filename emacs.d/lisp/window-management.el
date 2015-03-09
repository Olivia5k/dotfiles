;;; i3 window manager hacks

(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x l") 'windmove-right)

(global-set-key (kbd "C-x C-j")
                (lambda ()
                  (interactive)
                  (split-window-below)
                  (windmove-down)))

(global-set-key (kbd "C-x C-l")
                (lambda ()
                  (interactive)
                  (split-window-right)
                  (windmove-right)))

(global-set-key (kbd "C-q") 'delete-window)

(global-set-key (kbd "M-<F13>") 'windmove-up)
(global-set-key (kbd "M-<F14>") 'windmove-down)
(global-set-key (kbd "M-<F15>") 'windmove-left)
(global-set-key (kbd "M-<F16>") 'windmove-right)

(provide 'window-management)
