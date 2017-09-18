(use-package lispy
  :bind
  (:map lispy-mode-map
        ("D" . th/lispy-to-defun)))

(use-package stumpwm-mode)

(defun th/lispy-to-defun ()
  (interactive)
  (if (lispy-left-p)
      (lispy-to-defun)
    (insert "D")))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'lispy-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'rainbow-identifiers-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'stumpwm-mode)

(defun th/buffer-or-region (action-name buffer-func region-func)
  (let ((s "Buffer"))
    (if (use-region-p)
        (progn
          (funcall region-func (region-beginning) (region-end))
          (keyboard-escape-quit)
          (setq s "Region"))
      (funcall buffer-func))

    (message "buffer-or-region: %s %s" s action-name)))

(defun eval-buffer-or-region ()
  (interactive)
  (th/buffer-or-region "eval" 'eval-buffer 'eval-region))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer-or-region)

(provide 'th-elisp)
