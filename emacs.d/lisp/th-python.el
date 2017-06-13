(use-package python-mode
  :config
  (add-hook 'python-mode-hook 'flycheck-mode)
  (setq-default py-split-windows-on-execute-p nil))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package pip-requirements)


(provide 'th-python)
