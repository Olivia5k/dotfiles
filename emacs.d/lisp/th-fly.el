(use-package flycheck
  :bind
  ("C-x C-n" . flycheck-next-error)
  ("C-x C-p" . flycheck-previous-error)

  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


;; Spell checking with flyspell
(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'text-mode-hook (lambda () (flyspell-mode)))

(provide 'th-fly)
