(use-package docker
  :bind (("C-x C-d" . docker-containers)
         (:map docker-containers-mode-map
               ("i" . docker-images))))
(use-package dockerfile-mode
  :bind (:map dockerfile-mode-map
              ("C-c C-c" . th/docker-compose)))
(use-package docker-tramp)

(defun th/docker-compose ()
  "Run docker compose on the current file"
  (interactive)
  (compile "docker-compose build"))

(provide 'th-docker)
