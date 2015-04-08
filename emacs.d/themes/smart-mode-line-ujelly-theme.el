(deftheme smart-mode-line-ujelly "Jellybeans theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-ujelly
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background "#222229"))) 
 '(mode-line-inactive ((t :foreground "gray60" :background "#202028" :inverse-video nil)))
 '(mode-line     ((t :foreground "gray60" :background "#303039" :inverse-video nil)))
 '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "White")))
 '(sml/filename  ((t :inherit sml/global :foreground "#eab700" :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "#bf6000")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
 '(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
 '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-ujelly)
