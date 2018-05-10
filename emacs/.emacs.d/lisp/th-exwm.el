(use-package exwm)

(server-start)

(setq exwm-workspace-number 10)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (string= "gimp" exwm-instance-name)
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; `exwm-input-set-key' allows you to set a global key binding (available in
;; any case). Following are a few examples.
;; + We always need a way to go back to line-mode from char-mode
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; + Bind a key to switch workspace interactively
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; + Bind "s-0" to "s-9" to switch to the corresponding workspace.
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
;; + Application launcher ('M-&' also works if the output buffer does not
;;   bother you). Note that there is no need for processes to be created by
;;   Emacs.
(defun exwm-execute (command)
  (interactive
   (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

;; Terminal launcher
(defun th/ansi-term ()
  (interactive)
  (let ((buf (get-buffer "*ansi-term*")))
    (if buf
        (switch-to-buffer buf)
      (ansi-term "/usr/bin/zsh"))))

(exwm-input-set-key (kbd "s-<return>") #'th/eshell-here)
(exwm-input-set-key (kbd "M-s-<return>") #'th/ansi-term)
(exwm-input-set-key (kbd "C-M-s-<return>") (lambda () (interactive) (start-process-shell-command
                                     "urxvt" nil "urxvt")))

;; + 'slock' is a simple X display locker provided by suckless tools.
(exwm-input-set-key (kbd "s-<backspace>") 'lock)
(defun lock ()
  (interactive)
  (start-process "" nil "lock"))

(defun th/browser-golden ()
  "Splits the current window into a browser at 2/3 of the window"
  (interactive)
  (delete-other-windows)
  ;; Add one sixth to make it go from 1/2 to 1/3
  (let* ((width (/ (window-width) 5)))
    (split-window-right)
    (other-window 1)
    (enlarge-window-horizontally width)
    (start-process-shell-command "firefox" nil "firefox")))

(defun th/goto-browser ()
  "Run or raise a browser in the current frame.

If there are multiple, complete for them."
  (interactive)
  (let* ((browser-buffers (--map (buffer-name it)
                                 (--filter (s-prefix? "Firefox" (buffer-name it))
                                  (buffer-list)))))
    (cond
     ((= (length browser-buffers) 1)
      (switch-to-buffer (car browser-buffers)))
     ((> (length browser-buffers) 1)
      (switch-to-buffer (switch-to-buffer
                         (completing-read "Select firefox: " browser-buffers))))
     (t
      (start-process-shell-command "firefox" nil "firefox")))))

(exwm-input-set-key (kbd "s-h") 'windmove-left)
(exwm-input-set-key (kbd "s-j") 'windmove-down)
(exwm-input-set-key (kbd "s-k") 'windmove-up)
(exwm-input-set-key (kbd "s-l") 'windmove-right)

(exwm-input-set-key (kbd "s-M-h") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-M-j") 'shrink-window)
(exwm-input-set-key (kbd "s-M-k") 'enlarge-window)
(exwm-input-set-key (kbd "s-M-l") 'enlarge-window-horizontally)

(exwm-input-set-key (kbd "s-M-C-h") (lambda () (interactive) (shrink-window-horizontally 3)))
(exwm-input-set-key (kbd "s-M-C-j") (lambda () (interactive) (shrink-window 3)))
(exwm-input-set-key (kbd "s-M-C-k") (lambda () (interactive) (enlarge-window 3)))
(exwm-input-set-key (kbd "s-M-C-l") (lambda () (interactive) (enlarge-window-horizontally 3)))

(exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)
(exwm-input-set-key (kbd "s-M-b") 'balance-windows)
(exwm-input-set-key (kbd "s-<tab>") 'th/switch-to-previous-buffer)

(exwm-input-set-key (kbd "s-n") 'flycheck-next-error)
(exwm-input-set-key (kbd "s-p") 'flycheck-previous-error)
(exwm-input-set-key (kbd "s-M-C-n") 'first-error)
(exwm-input-set-key (kbd "s-M-n") 'next-error)
(exwm-input-set-key (kbd "s-M-p") 'previous-error)

(exwm-input-set-key (kbd "s-M-b") 'balance-windows)

(exwm-input-set-key (kbd "s-C-a") 'org-build-agenda)
(exwm-input-set-key (kbd "s-o") 'th/org/body)
(exwm-input-set-key (kbd "s-m") 'mu4e-hydra/body)

(exwm-input-set-key (kbd "s-.") (lambda () (interactive) (message (format-time-string "%Y-%m-%d %T (%a w%W)"))))

(exwm-input-set-key (kbd "C-s-p") (lambda () (interactive) (start-process-shell-command "ss" nil "ss -s")))
(exwm-input-set-key (kbd "C-M-s-p") (lambda () (interactive) (start-process-shell-command "ss" nil "ss")))

(exwm-input-set-key (kbd "s-q") #'th/goto-browser)
(exwm-input-set-key (kbd "C-s-q") #'th/browser-golden)
(exwm-input-set-key (kbd "C-s-s") (lambda () (interactive) (exwm-execute "spotify")))

(exwm-input-set-key (kbd "s-SPC") 'exwm-execute)

(defun th/switch-screens ()
  "Switch screen setup."
  (interactive)
  (let* ((arg (completing-read "screen mode: " '(laptop work desktop tv))))
    (cond
     ((s-equals? arg "laptop")
      (shell-command "xrandr --output eDP1 --auto")
      (shell-command "xrandr --output HDMI2 --off"))

     ((s-equals? arg "work")
      (shell-command "xrandr --output HDMI2 --right-of eDP1 --auto"))

     ((s-equals? arg "desktop")
      (shell-command "xrandr --output HDMI-0 --off")
      (shell-command "xrandr --output DVI-I-1 --left-of DVI-D-0"))

     ((s-equals? arg "tv")
      (shell-command "xrandr --output HDMI-0 --auto")
      (shell-command "xrandr --output HDMI-0 --right-of DVI-D-0")))

    (shell-command "keyboard-setup")
    (exwm-randr--refresh)))

(exwm-input-set-key (kbd "s-<prior>") (lambda () (interactive)
                       (shell-command "keyboard-setup")
                       (message "a6 loaded")))
(exwm-input-set-key (kbd "s-<next>") (lambda () (interactive)
                      (shell-command "keyboard-setup us")
                      (message "us loaded")))

(defun th/exwm-chrome (url name)
  "Open a Chrome app for `url'"
  (interactive)
  (start-process "" name "browser-app" url))

;; Volume control!
(exwm-input-set-key (kbd "s-z") (lambda () (interactive) (start-process-shell-command "pulsemixer" nil
                                                      "pulsemixer --toggle-mute")))

;; The following example demonstrates how to set a key binding only available
;; in line mode. It's simply done by first push the prefix key to
;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
;; The example shorten 'C-c q' to 'C-q'.
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.
(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)
   ([?\C-d] . delete)))

;; You can hide the mode-line of floating X windows by uncommenting the
;; following lines
(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line
(setq exwm-workspace-minibuffer-position nil)

(defun exwm-randr-dragonwing ()
  (start-process-shell-command
   "xrandr" nil "xrandr --output HDMI2 --right-of eDP1 --auto"))

(defun exwm-randr-dragonisle ()
  (start-process-shell-command
   "xrandr" nil "xrandr --output DP-0 --right-of HDMI-0 --auto"))

(require 'exwm-randr)
(require 'th-exwm-workspace)

(cond
 ((s-equals? (system-name) "dragonisle")
  (th/ew/setup
   '("HDMI-0" "DP-0")
   '("drunkenfall" "conf")
   #'exwm-randr-dragonisle))

 ((s-equals? (system-name) "dragonwing")
  (th/ew/setup
   '("eDP1" "HDMI-2")
   '("unomaly" "conf")
   #'exwm-randr-dragonwing)))

;; Whenever the screen refreshes, make sure to reset the keyboard layout
;; TODO(thiderman): Temporarily disabled since it seems to cause lag
;; (add-hook 'exwm-randr-refresh-hook
;;           (lambda ()
;;             (start-process-shell-command
;;                "" nil "keyboard-setup")))

(exwm-randr-enable)
;; Do not forget to enable EXWM. It will start by itself when things are ready.
(exwm-enable)

(provide 'th-exwm)
