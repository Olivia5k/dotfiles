(use-package ace-window
  :bind ("s-w" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  :bind
  ("M-s-n" . winner-undo)
  ("M-s-p" . winner-redo)
  :ensure nil
  :config (winner-mode 1))

;; Try to make emax split vertically when possible
(setq split-height-threshold 100)
(setq split-width-threshold 160)

;; So that (compile) and other commands re-use already open buffers in other
;; frames. Really useful when using dual monitors.
(setq display-buffer-reuse-frames t)


(defun th/split-horizontally ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (balance-windows))

(defun th/split-vertically ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (balance-windows))


(defadvice customize-group (before customize-group-split-window activate)
  "Makes `customize-group' appear in its own window."
  (split-window-sensibly)
  (other-window 1))

(defadvice split-window-sensibly (after split-window-sensibly-autobalance activate)
  (balance-windows))

(defun split-window-sensibly (&optional window)
  "A split-window-sensibly that's actually sensible.

If there is room for a window (80 chars) to the side, split to
there. Otherwise split downwards.

Checks `window-splittable-p' as well so that windows that already have
a fixed size can retain that fixed size. `magit-popup' is a common use
case for this."
  (let ((window (or window (selected-window))))
    (if (or (not (window-splittable-p window t))
            (not (>= (window-width window) 160)))
        (split-window-vertically)
      (split-window-horizontally))))

(defun th/kill-window ()
  "Kill the window. If it's the last one in the frame and the server is running, kill the frame."
  (interactive)
  (if (and (one-window-p) (server-running-p))
      (progn
        (when (buffer-file-name)
          (save-buffer))
        (delete-frame))
    (delete-window)
    (save-excursion
      (balance-windows))))

(global-set-key (kbd "C-q") 'th/kill-window)


;; http://oremacs.com/2015/02/04/pre-hydra-post/
(defhydra hydra-window (:columns 4)
  "window"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" shrink-window-horizontally)
  ("J" shrink-window)
  ("K" enlarge-window)
  ("L" enlarge-window-horizontally)
  ("f" projectile-find-file "file" :color blue)
  ("p" projectile-switch-project "project" :color blue)
  ("b" balance-windows "balance")
  ("e" next-error "next error")
  ("E" previous-error "prev error")
  ("M-e" first-error "first error")
  ("n" new-frame "new frame" :color blue)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)
         (throw 'hydra-disable t))
   "ace")
  ("s" th/split-vertically "vert")
  ("z" th/split-horizontally "horz")
  ("w" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)
         (throw 'hydra-disable t))
   "swap")
  ("t" transpose-frame "'")
  ("d" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)
         (balance-windows)
         (throw 'hydra-disable t))
   "del")
  ("o" delete-other-windows "one" :color blue)
  ("i" ace-maximize-window "ace-one" :color blue)
  ("q" nil "cancel"))

(defun th/copy-or-hydra-window ()
  "M-w without a region is terrible. Make it useful instead."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (hydra-window/body)))

(global-set-key (kbd "M-w") 'th/copy-or-hydra-window)

(defmacro stumpwm-emacs-cmd (emacs-fun stump-cmd)
  (let ((cmd-name (intern (format "stumpwm/%s" emacs-fun))))
    `(defun ,cmd-name ()
       (interactive)
       ;; If these errors are ignored, the command failover won't
       ;; work. This leads to annoying "(Shell command failed with
       ;; code 1 and no output)" messages. Can't seem to be rid of
       ;; them. :(
       (unless (ignore-errors (funcall ',emacs-fun))
         (shell-command (format "stumpish %s" ,stump-cmd))))))

(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'th/split-horizontally)
(global-set-key (kbd "M-3") 'th/split-vertically)


(provide 'th-window-management)
