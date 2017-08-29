(defun make-rect (color height width)
  "Create an XPM bitmap."
  (when window-system
    (propertize
     " " 'display
     (let ((data nil)
           (i 0))
       (setq data (make-list height (make-list width 1)))
       (pl/make-xpm "percent" color color (reverse data))))))

(defun powerline-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))

;; (defvar powerline-center-components nil)
;; (setq powerline-center-components nil)

;; ;;;###autoload
;; (defmacro powerline-center-component (target pred eval)
;;   "Register a component that overrides the major mode display in the modeline."
;;   `(setq powerline-center-components
;;         (plist-put powerline-center-components
;;                    (quote ,target)
;;                    (list (quote ,pred)
;;                          (list (quote ,eval))))))

;; (powerline-center-component ace
;;  (-contains? (list-minor-modes) 'ace-window-mode)
;;  (" " ace-window-mode " "))

;; (powerline-center-component mu4e
;;  (eq major-mode 'mu4e-headers-mode)
;;  (" " mu4e~headers-last-query " "))

;; (defmacro powerline-center-cond ()
;;   "Renders the conditional center component.

;; Based on the contents of `powerline-center-components'."
;;   `(cond
;;     ,@(-mapcat
;;        (lambda (x) (when (listp x) (list x)))
;;        powerline-center-components)
;;     (t
;;      (list
;;       " "
;;       (powerline-mode-icon)
;;       " %["
;;       (powerline-major-mode)
;;       "%] "))))

(use-package powerline
  :ensure t
  :config
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (modified (buffer-modified-p))
                          (ro buffer-read-only)
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (bar-color (cond ((and active modified) (face-foreground 'error))
                                           (active (face-background 'cursor))
                                           (t (face-background 'tooltip))))
                          (lhs (list
                                (make-rect bar-color 30 3)
                                ;; If read-only, show the lock icon. If not
                                ;; read-only and modified, show the modified icon.
                                ;; If buffer is not tied to a file, don't show anything.
                                (if (buffer-file-name)
                                    (if ro
                                        (concat
                                         " "
                                         (all-the-icons-faicon "ban"
                                                               :face (when active 'font-lock-keyword-face)
                                                               :v-adjust -0.01))
                                      (when modified
                                        (concat
                                         " "
                                         (all-the-icons-faicon "floppy-o"
                                                               :face (when active 'error)
                                                               :v-adjust -0.01)))))
                                " "
                                (powerline-buffer-id)
                                " "))

                          (center (cond
                                   ((-contains? (list-minor-modes) 'ace-window-mode)
                                    (list " " ace-window-mode " "))

                                   ((eq major-mode 'mu4e-headers-mode)
                                    (list " " mu4e~headers-last-query " "))

                                   (t
                                    (list
                                     " "
                                     (powerline-mode-icon)
                                     " "
                                     (powerline-major-mode)
                                     " "))))
                          (rhs (list
                                ;; " "
                                ;; (powerline-minor-modes)
                                " "
                                (powerline-raw " %l:%c" 'mode-line 'r)
                                " | "
                                (powerline-raw "%6p" 'mode-line 'r)
                                (powerline-hud 'highlight 'region 1)
                                " "
                                )))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face2 (/ (powerline-width center) 2.0))
                      (powerline-render center)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))

(provide 'th-modeline)
