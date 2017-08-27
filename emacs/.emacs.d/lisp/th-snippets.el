(use-package yasnippet
  :demand
  :config
  (yas-reload-all) ;; Without this, it doesn't load...
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'markdown-mode 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode))

(defhydra th/yas-hydra (:exit t)
  "yas"
  ("s" yas-insert-snippet "snippet")
  ("M-s" yas-insert-snippet "snippet")
  ("c" yas-new-snippet "new")
  ("n" yas-new-snippet "new")
  ("v" yas-visit-snippet-file "visit"))

(global-set-key (kbd "M-s") 'th/yas-hydra/body)

(provide 'th-snippets)
