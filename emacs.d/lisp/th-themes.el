(require 'linum)

;; Load the theme real early
(use-package darktooth-theme
  :ensure t)
(load-theme 'darktooth)


;; all-the-icons - fantastic icons package <3
(use-package all-the-icons
  :config
  (use-package all-the-icons-dired
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

;; Automatic bolding of certain keywords
(defun th/boldly-go ()
  "Fix bolding in colorschemes"

  (let ((faces (face-list)))
    (mapc
     (lambda (face)
       (set-face-attribute face nil ':bold t))
     (-filter
      (lambda (face)
        (let ((fn (symbol-name face)))
          (or (s-starts-with? "font-lock" fn)
              (s-starts-with? "org-level" fn)
              (s-starts-with? "rainbow" fn))))
      faces))

    (mapc
     (lambda (face)
       (set-face-attribute face nil ':inverse-video t))
     (-filter
      (lambda (face)
        (let ((fn (symbol-name face)))
          (s-starts-with? "avy-lead" fn)))
      faces))))

(defadvice load-theme (after load-theme-auto-bold activate)
  (th/boldly-go))

(defun th/fix-darktooth ()
  "Things that make darktooth a theme more suited for me."

  (th/boldly-go)

  ;; Also make the linum and the fringe stand out more!
  (set-face-attribute 'linum nil :background "#1D2021" :foreground "#7C6F64")
  (set-face-attribute 'fringe nil :background "#3C3836")

  ;; Also strings and types are the same, which is bad. Make strings greener.
  (set-face-attribute 'font-lock-string-face nil :foreground "#427B58")
  ;; And functions do not stand out at all...
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#FE8019")

  (message "darktooth fixed"))

(th/fix-darktooth)
(provide 'th-themes)
