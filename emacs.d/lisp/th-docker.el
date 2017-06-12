(use-package docker
  :bind (("C-x C-d" . docker-containers)
         (:map docker-containers-mode-map
               ("i" . docker-images))))
(use-package dockerfile-mode)
(use-package docker-tramp)

(provide 'th-docker)
