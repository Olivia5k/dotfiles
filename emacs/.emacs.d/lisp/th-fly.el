(use-package flycheck
  :bind
  ("C-x C-n" . flycheck-next-error)
  ("C-x C-p" . flycheck-previous-error)

  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


(defhydra hydra-flycheck (:color blue)
  "
^
^Flycheck^        ^Errors^          ^Checker^
^────────^────────^──────^──────────^───────^───────────
_q_ quit          _c_ check         _s_ select
_v_ verify setup  _n_ next          _d_ disable
_m_ manual        _p_ previous      _?_ describe
                _l_ list
^^                  ^^                  ^^
"
  ("q" nil)
  ("c" flycheck-buffer)
  ("d" flycheck-disable-checker)
  ("l" flycheck-list-errors :color red)
  ("m" flycheck-manual)
  ("n" flycheck-next-error :color red)
  ("p" flycheck-previous-error :color red)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup)
  ("?" flycheck-describe-checker))

(use-package flyspell
  :bind
  ("C-;" . iedit-mode)
  :config
  ;; (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'text-mode-hook (lambda () (flyspell-mode))))

(provide 'th-fly)
