(require 'dash)

;; Workspace management
(exwm-input-set-key (kbd "s-a") (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-d") (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-x") (lambda () (interactive) (exwm-workspace-switch 6)))
(exwm-input-set-key (kbd "s-SPC") #'th/ew/hydra/body)

(defhydra th/ew/hydra (:exit t)
  "exwm"
  ("s-SPC" th/ew/switch "workspace"))

(defvar th/ew/current "main")
(defvar th/ew/screens '("HDMI-0"))
(defvar th/ew/workspaces '("main"))

;; TODO(thiderman): Convert to dynamic amount of screens
;; TODO(thiderman): Make into a hook that sets up based on amount of workspaces
(defun th/ew/plist (left right)
  "Returns workspace plist where evens are to the left and vice versa"
  (let (wp)
    (dotimes (i 20)
      (setq wp (plist-put wp i (if (evenp i) left right))))
    wp))

(defun th/ew/setup (screens spaces hook)
  "Configure workspace variables.

`screens' is a list of randr outputs to configure
`spaces' is the name of the different workspaces
`hook' is a randr hook to be run on screen change"

  (setq th/ew/screens screens)
  (setq exwm-randr-workspace-output-plist
        (th/ew/plist
         (car screens)
         (cadr screens)))
  (setq th/ew/workspaces spaces)
  (setq th/ew/current (car spaces))
  (add-hook 'exwm-randr-screen-change-hook hook))

(defun th/ew/name (index)
  "Return the name of the workspace at `index'"
  (nth index th/ew/workspaces))

(defun th/ew/name-for-exwm-workspace (index)
  "Return the name of the exwm workspace at `index'"
  (th/ew/name (/ index (length th/ew/screens))))

(defun th/ew/index (name)
  "Return the index of the workspace named `name'"
  (-elem-index name th/ew/workspaces))

(defun th/ew/workspace-index (&optional name)
  "Return the starting exwm workspace index of the workspace named `name'"
  (let ((name (or name th/ew/current)))
    (* (length th/ew/screens)
       (th/ew/index name))))

(defun th/ew/goto (name)
  (setq th/ew/current name)
  (let ((ws (th/ew/workspace-index name)))
    (exwm-workspace-switch ws)
    (exwm-workspace-switch (+ ws 1)))
  (th/ew/hud))

(defun th/ew/hud ()
  "Display a status bar with workspaces, with the current one highlighted"
  (message
   "%s"
   (s-join (propertize " / " 'face 'font-lock-comment-delimiter-face)
           (-map
            (lambda (x)
              (if (string-equal x th/ew/current)
                  (propertize x 'face 'font-lock-keyword-face)
                x))
            th/ew/workspaces))))

;;;###autoload
(defun th/ew/switch (&optional name)
  "Goto or create workspace `name'"
  (interactive)
  (let ((name (or name (completing-read "workspace: " th/ew/workspaces))))
    (add-to-list 'th/ew/workspaces name t)
    (th/ew/goto name)))

(defun th/ew/next ()
  "Go to the next workspace, looping back if stepping over the last"
  (interactive)
  (th/ew/goto (th/ew/shift 1)))

(defun th/ew/previous ()
  "Go to the previous workspace, looping back if stepping over the first"
  (interactive)
  (th/ew/goto (th/ew/shift -1)))

(defun th/ew/shift (shift)
  "Step `shift' steps in workspaces, looping back if stepping over the last"
  ;; TODO(thiderman): Currently only works in steps of one, but should
  ;; be good enough. Who steps multiple? :thonk:
  (let* ((current (-elem-index th/ew/current th/ew/workspaces))
         (ws (length th/ew/workspaces))
         (target (+ shift current))
         (final (cond
                 ;; If we are on a negative target, we should just go
                 ;; to the last
                 ((= target -1)
                  (- ws 1))
                 ;; If we are moving outside, go back to the first
                 ((= ws target)
                  0)
                 (t
                  target))))
    (nth final th/ew/workspaces)))

(exwm-input-set-key (kbd "s-p") #'th/ew/next)
(exwm-input-set-key (kbd "s-n") #'th/ew/previous)

(provide 'th-exwm-workspace)
