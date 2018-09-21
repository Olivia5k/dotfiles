(use-package docker
  :bind (("C-x C-d" . docker-containers)
         ;; (:map docker-containers-command-map
         ;;       ("i" . docker-images))
         ))
(use-package dockerfile-mode
  :bind (:map dockerfile-mode-map
              ("C-c C-c" . th/docker-compose)))
(use-package docker-tramp)

(use-package docker-compose-mode
  :straight (docker-compose-mode
             :host github
             :repo "thiderman/docker-compose-mode"
             :branch "executor"
             :upstream (:host github :repo "meqif/docker-compose-mode"))
  :bind (:map docker-compose-mode-map
              ("C-c C-c" . docker-compose-execute-command)
              ("C-c C-b" . docker-compose-build-buffer)
              ("C-c C-s" . docker-compose-start-buffer))
  :init
  (setq docker-compose-commands
        (list
         "build"
         "build --no-cache"
         "logs --follow"
         "logs --follow --tail=100"
         "up"
         "up --build"
         "create"
         "down"
         "exec"
         "images"
         "logs"
         "ps"
         "pull"
         "push"
         "restart"
         "rm"
         "rm -f"
         "run"
         "start"
         "stop"
         "top"
         )))

(defun th/docker-compose ()
  "Run docker compose on the current file"
  (interactive)
  (compile "make docker"))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(provide 'th-docker)
