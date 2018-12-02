(require 'url)

(use-package emojify
  :config
  (global-emojify-mode 1))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(defhydra th/chat (:exit t)
  "chat"
  ("s-e" (switch-to-buffer "&bitlbee") "bitlbee buffer")
  ("s" slack-start "start slack")
  ("b" (circe "BitlBee") "start bitlbee")
  ("i" insert-image-from-url "view last image")
  ("m" emojify-insert-emoji "emoji 💜")
  ("c" slack-select-rooms "slack"))

(global-set-key (kbd "s-e") 'th/chat/body)

(defadvice tracking-next-buffer (before tracking-small-windows activate)
  "Make chat buffers appear in smaller splits"
  (when (not (memq major-mode '(circe-query-mode
                                circe-channel-mode
                                slack-message-buffer-mode
                                slack-mode)))
    (split-window-below -20)
    (windmove-down)))

;; Load secret credentials and start the chats
;; (add-to-list 'load-path "/home/thiderman/src/github.com/thiderman/secrets/emacs/.emacs.d/lisp")
;; (load "th-chat-secret" t t)

(provide 'th-chat)
