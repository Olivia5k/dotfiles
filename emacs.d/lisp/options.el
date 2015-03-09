;;; Default and miscellaneous options

(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(tool-bar-mode -1)
(mouse-wheel-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

(setq backup-inhibited t
      auto-save-default t)

(setq backup-by-copying t)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (format "%s/%s-%s/" temporary-file-directory "emacs" (user-uid)))
(make-directory emacs-tmp-dir t)
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(setq auto-save-interval 1
      auto-save-timeout 1)

(setq echo-keystrokes 0.4
      debug-on-error nil
      stack-trace-on-error nil
      standard-indent 4
      tab-always-indent 'complete
      grep-scroll-output t)

(setq-default comment-column 42
              fill-column 78
              indent-tabs-mode nil
              tab-width 4
              word-wrap t)

(show-paren-mode t)
(global-auto-revert-mode t)

(provide 'options)
