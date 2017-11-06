(require 'url)

(use-package emojify
  :config
  (global-emojify-mode 1))

(defun insert-image-from-url (&optional url)
  (interactive)
  (save-excursion
    (unless url
      (search-backward "http")
      (setq url (url-get-url-at-point)))
    (unless url
      (error "Couldn't find URL.")))

  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
         (let ((data (with-current-buffer buffer
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max)))))
           (split-window-below)
           (windmove-down)
           (switch-to-buffer "*image*")
           (local-set-key (kbd "q") 'th/kill-window)
           (erase-buffer)
           (insert-image (create-image data nil t)))
      (kill-buffer buffer))))



(defun th/circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) ">")
                       'face 'circe-prompt-face)
           " ")))

(use-package circe
  :bind
  (:map circe-query-mode-map
        ("M-m" . emojify-insert-emoji))
  (:map circe-channel-mode-map
        ("M-m" . emojify-insert-emoji))
  :config
  (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
  (circe-set-display-handler "PART" (lambda (&rest ignored) nil))
  (circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))

  (load "lui-logging" nil t)
  (setq lui-logging-directory "~/.logs/circe/")
  (make-directory lui-logging-directory t)
  (enable-lui-logging-globally)

  (add-hook 'circe-chat-mode-hook 'th/circe-prompt)

  (tracking-mode 1))


(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)

  (setq tracking-ignored-buffers
        '("#gitlab"
          "#notifications"
          "#support"
          "#infrastructure")))

;; omfg how can the buffer names be so long by default
(defmethod slack-room-buffer-name ((room slack-channel))
  (concat "#" (slack-room-display-name room)))

(defmethod slack-room-buffer-name ((room slack-group))
  (concat (slack-room-display-name room)))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))


(defhydra th/chat (:exit t)
  "chat"
  ("s-e" (switch-to-buffer "&bitlbee") "bitlbee")
  ("i" insert-image-from-url "view last image")
  ("m" emojify-insert-emoji "emoji 💜")
  ("c" slack-select-rooms "slack"))

(global-set-key (kbd "s-e") 'th/chat/body)


(defadvice tracking-next-buffer (before tracking-small-windows activate)
  "Make chat buffers appear in smaller splits"
  (when (not (memq major-mode '(circe-query-mode
                                circe-channel-mode
                                slack-mode)))
    (split-window-below -20)
    (windmove-down)))

;; Load secret credentials and start the chats

(add-to-list 'load-path "/home/thiderman/src/github.com/thiderman/secrets/emacs/.emacs.d/lisp")
(load "th-chat-secret" nil t)

(provide 'th-chat)
