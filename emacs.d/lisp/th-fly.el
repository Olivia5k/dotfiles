(use-package flycheck
  :bind
  ("C-x C-n" . flycheck-next-error)
  ("C-x C-p" . flycheck-previous-error)

  :config
  (setq flycheck-checkers '(css-csslint emacs-lisp go-gofmt go-golint go-vet go-build go-test html-tidy json-jsonlint json-python-json less python-flake8 python-pylint python-pycompile sass scss-lint scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck sql-sqlint yaml-jsyaml yaml-ruby))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flyspell
  :bind
  ("C-;" . iedit-mode)
  :config
  (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'text-mode-hook (lambda () (flyspell-mode))))



(provide 'th-fly)
