(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all) ;; Without this, it doesn't load...
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'markdown-mode 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; Bound to be used in the toggle hydra
(defhydra th/yas-hydra (:exit t)
  "yas"
  ("C-s" yas-insert-snippet "snippet")
  ("s" yas-insert-snippet "snippet")
  ("c" yas-new-snippet "new")
  ("n" yas-new-snippet "new")
  ("v" yas-visit-snippet-file "visit"))

(provide 'th-snippets)
