(defun th/modeline-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))

(use-package telephone-line
  :init
  (telephone-line-defsegment th/telephone-buffer-segment ()
    (list
     (when (buffer-file-name)
       (if buffer-read-only
           (all-the-icons-faicon "ban"
                                 :face 'font-lock-keyword-face
                                 :v-adjust -0.01)
         (when (buffer-modified-p)
           (all-the-icons-faicon "floppy-o"
                                 :face 'error
                                 :v-adjust -0.01))))
     " "
     (buffer-name)))

  (telephone-line-defsegment th/telephone-clock-segment ()
    (when (telephone-line-selected-window-active)
      (if (org-clocking-p)
          (org-clock-get-clock-string)
        "<not clocking>")))

  (telephone-line-defsegment* th/vc-segment ()
    (telephone-line-raw
     (replace-regexp-in-string "git." "" (substring-no-properties (if vc-mode vc-mode "")) t t) t))

  (setq telephone-line-faces
        '((accent . (telephone-line-accent-active . telephone-line-accent-inactive))
          (nil    . (mode-line . mode-line-inactive))))

  (setq telephone-line-lhs
        '((nil    . (telephone-line-process-segment))
          (accent . (telephone-line-major-mode-segment))
          (nil    . (th/telephone-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (th/telephone-clock-segment))
          (nil    . (th/vc-segment))
          (accent . (telephone-line-minor-mode-segment))
          (nil    . (telephone-line-airline-position-segment))))

  :config
  (telephone-line-mode 1))

(provide 'th-modeline)
