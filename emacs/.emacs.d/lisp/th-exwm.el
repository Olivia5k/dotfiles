(use-package exwm
  :straight (exwm :type git :host github :repo "ch11ng/exwm"))

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
                      (string= "gimp" exwm-instance-name)
                      (string= "qutebrowser" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Set input mode to be char for terminals
(setq exwm-manage-configurations
      '(((string= "kitty" exwm-instance-name)
         char-mode t)))

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
(exwm-input-set-key (kbd "M-s-<return>") (lambda () (interactive) (th/eshell-here 1)))
(exwm-input-set-key (kbd "C-M-s-<return>") #'th/goto-terminal)

;; + 'slock' is a simple X display locker provided by suckless tools.
(exwm-input-set-key (kbd "s-<backspace>") 'lock)
(defun lock ()
  (interactive)
  (start-process "" nil "lock"))

(defun th/golden-split ()
  "Splits the current window into two, at a golden-ratio like"
  (delete-other-windows)
  ;; Add one fifth to make it go from 1/2 to 1/3, ish
  (let* ((width (/ (window-width) 5)))
    (split-window-right)
    (other-window 1)
    (enlarge-window-horizontally width)
    (set-frame-parameter nil 'th/prohibit-balance t)))

(defun th/browser-golden ()
  "Splits the current window into a browser at 2/3 of the window"
  (interactive)
  (th/golden-split)
  (th/goto-browser))

(defun th/goto-browser ()
  "Run or raise a browser in the current frame.

If there are multiple, complete for them."
  (interactive)
  (let* ((browser-buffers (--map (buffer-name it)
                                 (--filter (s-prefix? "Chromium" (buffer-name it))
                                  (buffer-list)))))
    (cond
     ((= (length browser-buffers) 1)
      (switch-to-buffer (car browser-buffers)))
     ((> (length browser-buffers) 1)
      (switch-to-buffer (switch-to-buffer
                         (completing-read "Select chromium: " browser-buffers))))
     (t
      (start-process-shell-command "chromium" nil "chromium")))))

(defun th/goto-terminal ()
  "Run or raise a terminal in the current frame.

If there are multiple, complete for them."
  (interactive)
  (let* ((browser-buffers (--map (buffer-name it)
                                 (--filter (s-prefix? "kitty" (buffer-name it))
                                  (buffer-list)))))
    (cond
     ((= (length browser-buffers) 1)
      (switch-to-buffer (car browser-buffers)))
     ((> (length browser-buffers) 1)
      (switch-to-buffer (switch-to-buffer
                         (completing-read "Select kitty: " browser-buffers))))
     (t
      (start-process-shell-command "kitty" nil "kitty")))))

(exwm-input-set-key (kbd "s-h") 'windmove-left)
(exwm-input-set-key (kbd "s-j") 'windmove-down)
(exwm-input-set-key (kbd "s-k") 'windmove-up)
(exwm-input-set-key (kbd "s-l") 'windmove-right)
(exwm-input-set-key (kbd "s-t") 'transpose-frame)

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

(exwm-input-set-key (kbd "s-M-C-n") 'first-error)
(exwm-input-set-key (kbd "s-M-n") 'next-error)
(exwm-input-set-key (kbd "s-M-p") 'previous-error)

(exwm-input-set-key (kbd "s-M-b") 'balance-windows)

(exwm-input-set-key (kbd "s-C-a") 'org-build-agenda)
(exwm-input-set-key (kbd "s-o") 'th/org/body)
(exwm-input-set-key (kbd "s-m") 'mu4e-hydra/body)

(defun th/exwm-date-bat ()
  (interactive)
  (message
   "%s %s %s"
   (format-time-string "%Y-%m-%d %T @ %a w%W [%s]")
   (format-time-string "(%T UTC)" nil t)
   (if (string-equal (system-name) "dragonwing")
       (format "    %s"
               (s-replace
                "\n" "; "
                (s-trim (shell-command-to-string "acpi -b"))))
     "")))

(exwm-input-set-key (kbd "s-.") #'th/exwm-date-bat)

(exwm-input-set-key (kbd "C-s-p") (lambda () (interactive) (start-process-shell-command "ss" nil "ss -s")))
(exwm-input-set-key (kbd "C-M-s-p") (lambda () (interactive) (start-process-shell-command "ss" nil "ss")))

(exwm-input-set-key (kbd "s-q") #'th/goto-browser)
(exwm-input-set-key (kbd "C-s-q") #'th/browser-golden)
(exwm-input-set-key (kbd "C-s-s") (lambda () (interactive) (exwm-execute "spotify")))

(exwm-input-set-key (kbd "M-s-SPC") 'exwm-execute)

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

(defun th/exwm-terminal (command)
  "Open a terminal with a dedicated command"
  (interactive
   (list (read-string "kitty: ")))
  (exwm-execute
   (format "kitty -e %s" command)))

;; Volume control!
(exwm-input-set-key (kbd "s-x") 'th/toggle-mute)

;; Splits
(exwm-input-set-key (kbd "s-z") #'th/split-horizontally)
(exwm-input-set-key (kbd "s-s") #'th/split-vertically)

;; Kill split
(exwm-input-set-key (kbd "C-s-k") 'delete-window)

(defun th/toggle-mute ()
  (interactive)
  (shell-command-to-string "pulsemixer --toggle-mute")
  (let ((muted (shell-command-to-string "pulsemixer --get-mute")))
    (message (if (string-equal (s-trim muted) "1") "muted" "not muted"))))

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

(defun exwm-randr-dragonisle ()
  (start-process-shell-command
   "xrandr" nil "xrandr --output DP-0 --right-of HDMI-0 --auto"))

(defun th/xrandr (&rest commands)
  (let ((command (format "xrandr --verbose --output %s" (s-join " --output " commands))))
    (message command)
    (start-process-shell-command "xrandr" "*xrandr*" command)))

(defun th/exwm-randr-hook ()
  (cond
   ((s-equals? (system-name) "dragonisle")
    (th/xrandr "DP-0 --right-of HDMI-0 --auto"
               "HDMI-0 --auto"))

   ((s-equals? (system-name) "dragonwing")

    (let ((screens (th/get-connected-screens)))
      (cond ((= 3 (length screens))
             ;; If we have three screens connected, that means that the laptop
             ;; screen is on and the two monitors are attached. Disable the
             ;; laptop screen and align the other two.
             (message "Triple screens")
             (th/xrandr "eDP1 --off"
                        "HDMI2 --right-of DP1 --auto"
                        "DP1 --auto")
             (setq exwm-randr-workspace-output-plist (th/ew/plist '("DP1" "HDMI2"))))

            ;; If we have two screens connected, it means we want to
            ;; show what's going on; mirror the laptop screen to the HDMI
            ((= 2 (length screens))
             (message "Two screens; mirroring")
             (th/xrandr "eDP1 --auto"
                        "HDMI2 --same-as eDP1 --mode 1920x1080")
             (setq exwm-randr-workspace-output-plist (th/ew/plist '("eDP1"))))
            ;; Just a laptop screen
            (t
             (message "One screen; laptop only")
             (th/xrandr "eDP1 --auto"
                        "HDMI2 --off"
                        "DP1 --off")
             (setq exwm-randr-workspace-output-plist (th/ew/plist '("eDP1")))))))))

(defun th/get-connected-screens ()
  "Returns a list of the screens xrandr reports as connected"
  (mapcar
   (lambda (s)
     ;; Hehe, ugly, but works since the outputs are always the first
     ;; thing on every row.
     (s-replace-regexp " .*" "" s))
   (s-split
    "\n"
    (s-trim (shell-command-to-string
             "xrandr --query | grep \" connected \"")))))

(require 'exwm-randr)
(require 'th-exwm-workspace)

(cond
 ((s-equals? (system-name) "dragonisle")
  (th/ew/setup
   (th/get-connected-screens)
   '("www" "drunkenfall")
   #'exwm-randr-dragonisle))

 ((s-equals? (system-name) "dragonwing")
  (th/ew/setup
   ;; If we have three screens, just use two of them
   (let ((screens (th/get-connected-screens)))
     (if (= 3 (length screens))
         (cdr screens)
       screens))
   '("www" "unomaly")
   #'th/exwm-randr-hook
   )))

;; Whenever the screen refreshes, make sure to reset the keyboard layout
;; TODO(thiderman): Temporarily disabled since it seems to cause lag
;; (add-hook 'exwm-randr-refresh-hook
;;           (lambda ()
;;             (start-process-shell-command
;;                "" nil "keyboard-setup")))

(exwm-randr-enable)
;; Do not forget to enable EXWM. It will start by itself when things are ready.
(exwm-enable)

(when (not (get-buffer "*redshift*"))
  (start-process-shell-command "redshift" "*redshift*" "redshift"))

(provide 'th-exwm)
