(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (setq css-indent-offset 2)
  (setq mmm-submode-decoration-level 0))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; If we're in vue-mode, make sure to reset it when we save.
  (add-hook 'after-save-hook
            (lambda ()
              (interactive)
              (when (eq 'vue-mode major-mode)
                (vue-mode)))))

(defun th/vue-switcher ()
  (interactive)
  (th/other-files-suffix "vue"))

(global-set-key (kbd "C-c v") #'th/vue-switcher)

(use-package emmet-mode
  :init
  (setq emmet-indentation 2)

  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)  ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)  ;; Enable Emmet's css abbreviation.
  (add-hook 'web-mode-hook  'emmet-mode))

(setq httpd-port 8001)

(provide 'th-web)
