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
  :config

  ;; (slack-register-team
  ;;  :name "emacs-slack"
  ;;  :default t
  ;;  :client-id "aaaaaaaaaaa.00000000000"
  ;;  :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  ;;  :token "aaaa-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
  ;;  :subscribed-channels '(test-rename rrrrr))

  ;; (slack-register-team
  ;;  :name "test"
  ;;  :client-id "3333333333.77777777777"
  ;;  :client-secret "cccccccccccccccccccccccccccccccc"
  ;;  :token "xxxx-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
  ;;  :subscribed-channels '(hoge fuga))
  )

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


(defhydra th/chat ()
  "chat"
  ("s-e" (message "chat buffers") "buffers")
  ("i" insert-image-from-url "view last image")
  ("m" emojify-insert-emoji "emoji 💜"))

(global-set-key (kbd "s-e") 'th/chat/body)

(provide 'th-chat)
