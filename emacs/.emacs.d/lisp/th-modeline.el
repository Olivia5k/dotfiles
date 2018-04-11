(defun th/modeline-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))

(defun th/tracking-status ()
  "Return the current track status.

This returns a list suitable for `mode-line-format'.

Modified version that does not do any properties."
  (if (not tracking-buffers)
      ""
    (let* ((buffer-names (cl-remove-if-not #'get-buffer tracking-buffers))
           (shortened-names (tracking-shorten tracking-buffers))
           (result (list " [")))
      (while buffer-names
        (push (car shortened-names)
              result)
        (setq buffer-names (cdr buffer-names)
              shortened-names (cdr shortened-names))
        (when buffer-names
          (push "," result)))
      (push "] " result)
      (nreverse result))))

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

  (telephone-line-defsegment th/telephone-tracking-segment ()
    (when (telephone-line-selected-window-active)
      (th/tracking-status)))

  (telephone-line-defsegment th/telephone-workspace ()
    (format "%d" exwm-workspace-current-index))

  (telephone-line-defsegment* th/vc-segment ()
    (telephone-line-raw
     (replace-regexp-in-string "git." "" (substring-no-properties (if vc-mode vc-mode "")) t t) t))

  (setq telephone-line-faces
        '((accent . (telephone-line-accent-active . telephone-line-accent-inactive))
          (track . (isearch-fail . mode-line-inactive))
          (nil    . (mode-line . mode-line-inactive))))

  (setq telephone-line-lhs
        '((accent . (telephone-line-process-segment))
          (nil    . (th/telephone-workspace))
          (accent . (telephone-line-major-mode-segment))
          (nil    . (th/telephone-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (th/telephone-clock-segment))
          (track . (th/telephone-tracking-segment))
          (nil    . (th/vc-segment))
          (accent . (telephone-line-minor-mode-segment))
          (nil    . (telephone-line-airline-position-segment))))

  (setq telephone-line-primary-left-separator telephone-line-utf-abs-left)
  (setq telephone-line-secondary-left-separator telephone-line-utf-abs-left)
  (setq telephone-line-primary-right-separator telephone-line-utf-abs-right)
  (setq telephone-line-secondary-right-separator telephone-line-utf-abs-right)

  :config
  (telephone-line-mode 1))

(provide 'th-modeline)
