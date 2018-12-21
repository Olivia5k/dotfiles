(add-hook 'emacs-lisp-mode-hook 'semantic-mode)
(add-hook 'go-mode-hook 'semantic-mode)
(add-hook 'python-mode-hook 'semantic-mode)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (auto-save-mode -1)))
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'fci-mode)

(add-hook 'prog-mode-hook 'abbrev-mode)

(provide 'th-hooks)
