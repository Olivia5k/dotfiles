(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  :bind
  ("M-s-n" . winner-undo)
  ("M-s-p" . winner-redo)
  :ensure nil
  :config (winner-mode 1))

(use-package shackle
  :config
  (setq shackle-default-rule '(:select t))
  (setq shackle-rules '((compilation-mode :noselect t)
                        (circe-query-mode :align t :size 0.3))))

;; Try to make emax split vertically when possible
(setq split-height-threshold 100)
(setq split-width-threshold 160)

;; So that `compile' and other commands re-use already open buffers in other
;; frames. Really useful when using dual monitors.
(setq display-buffer-reuse-frames t)

(defun th/balance-windows ()
  "Balance windows, but only if there are no X windows close by"
  (interactive)
  (when (not (frame-parameter nil 'th/prohibit-balance))
    (balance-windows)))

(window-list)

(defun th/toggle-prohibit-balance ()
  "Toggles if balance prohibiting is on or off"
  (interactive)
  (let ((target (not (frame-parameter nil 'th/prohibit-balance))))
    (set-frame-parameter nil 'th/prohibit-balance target)
    (message (if target "Prohibit enabled" "Prohibit disabled"))))

;;;###autoload
(defun th/split-horizontally ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (th/balance-windows))

;;;###autoload
(defun th/split-vertically ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (th/balance-windows))


(defadvice customize-group (before customize-group-split-window activate)
  "Makes `customize-group' appear in its own window."
  (split-window-sensibly)
  (other-window 1))

(defadvice split-window-sensibly (after split-window-sensibly-autobalance activate)
  (th/balance-windows))

(defun split-window-sensibly (&optional window)
  "A split-window-sensibly that's actually sensible.

If there is room for a window (80 chars) to the side, split to
there. Otherwise split downwards.

Checks `window-splittable-p' as well so that windows that already have
a fixed size can retain that fixed size. `magit-popup' is a common use
case for this."
  (let ((window (or window (selected-window))))
    (if (or (and (or (s-contains? "magit" (symbol-name major-mode))
                     (s-contains? "docker" (symbol-name major-mode)))
                 (not (window-splittable-p window t)))
            (not (>= (window-width window) 120)))
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
      (th/balance-windows))))

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
  ("b" th/balance-windows "balance")
  ("e" next-error "next error")
  ("E" previous-error "prev error")
  ("M-e" first-error "first error" :color blue)
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
  ("t" transpose-frame "transpose")
  ("d" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)
         (th/balance-windows)
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

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'th/split-horizontally)
(global-set-key (kbd "M-3") 'th/split-vertically)


(provide 'th-window-management)
