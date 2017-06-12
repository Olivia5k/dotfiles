(use-package vue-mode)

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; If we're in vue-mode, make sure to reset it when we save.
  (add-hook 'after-save-hook
            (lambda ()
              (interactive)
              (when (eq 'vue-mode major-mode)
                (vue-mode)))))

(global-set-key (kbd "C-c v")
                (lambda ()
                  (interactive)
                  (th/other-files-suffix "vue")))

(use-package emmet-mode
  :init
  (setq emmet-indentation 2)

  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)  ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)  ;; Enable Emmet's css abbreviation.
  (add-hook 'web-mode-hook  'emmet-mode))

(provide 'th-web)
