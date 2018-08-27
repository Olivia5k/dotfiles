(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(add-hook 'common-lisp-mode-hook 'lispy-mode)
(add-hook 'common-lisp-mode-hook 'paredit-mode)
(add-hook 'common-lisp-mode-hook 'eldoc-mode)
(add-hook 'common-lisp-mode-hook 'rainbow-identifiers-mode)
(add-hook 'common-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'common-lisp-mode-hook 'eros-mode)

(provide 'th-lisp)
