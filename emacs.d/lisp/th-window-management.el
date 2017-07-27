(use-package ace-window
  :demand
  :bind ("M-p" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  :bind
  ("M-s-n" . winner-undo)
  ("M-s-p" . winner-redo)
  ("C-c <left>" . th/disabled-key)
  ("C-c <right>" . th/disabled-key)
  :demand
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
    (progn
      (delete-window)
      (save-excursion
        (balance-windows)))))

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


(defmacro th/i3-emacs (win-fun i3-fun)
  "Generate a function that either runs WIN-FUN successfully or executes I3-FUN.

This can be used to have the same keybinding either move between
buffers in emacs, or execute an i3-msg command. So, if you try to move
left from a buffer that doesn't have anything to its left, `i3-msg
focus left` will be run instead.

This works best if used with https://github.com/thiderman/dotfiles/utils/i3emacs."

  (let ((cmd-name (intern (format "th/i3-emacs/%s" win-fun))))
    `(defun ,cmd-name ()
       (interactive)
       (unless (ignore-errors (funcall ',win-fun))
         (start-process-shell-command "i3-msg" nil (format "i3-msg %s" ,i3-fun))))))

(global-set-key (kbd "s-h") (th/i3-emacs windmove-left "focus left"))
(global-set-key (kbd "s-j") (th/i3-emacs windmove-down "focus down"))
(global-set-key (kbd "s-k") (th/i3-emacs windmove-up "focus up"))
(global-set-key (kbd "s-l") (th/i3-emacs windmove-right "focus right"))

;; Also disable the old ones so that I stop using them
(defun th/disabled-key ()
  (interactive)
  (message "This key is disabled."))

(global-set-key (kbd "C-x 1") #'th/disabled-key)
(global-set-key (kbd "C-x 2") #'th/disabled-key)
(global-set-key (kbd "C-x 3") #'th/disabled-key)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'th/split-horizontally)
(global-set-key (kbd "M-3") 'th/split-vertically)


(provide 'th-window-management)
