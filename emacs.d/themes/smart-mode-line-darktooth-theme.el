(deftheme smart-mode-line-darktooth "darktooth theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-darktooth
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background "#504945")))
 '(mode-line-inactive ((t :foreground "gray60" :background "#000000" :inverse-video nil)))
 '(mode-line     ((t :foreground "gray60" :background "#3c3836" :inverse-video nil)))
 '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "White")))
 '(sml/filename  ((t :inherit sml/global :foreground "#EBDBB2" :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "#8ec07c")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "#d3869b")))
 '(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
 '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-darktooth)
