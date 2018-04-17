(use-package filenotify)

(use-package compile
  :init
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

  (ignore-errors
    (require 'ansi-color)
    (defun colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

  ;; We usually want to see all of it
  (add-hook 'compilation-mode-hook (lambda () (toggle-truncate-lines -1)))

  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-auto-jump-to-first-error nil)
  (setq compilation-scroll-output t)
  (setq compilation-read-command nil))

(use-package makefile-executor
  :ensure t
  :load-path "/home/thiderman/src/github.com/thiderman/makefile-executor.el/"
  :commands (makefile-executor-execute-target makefile-executor-execute-project-target)
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package firestarter
  :config
  (firestarter-mode 1)
  (add-hook 'go-mode-hook
            (lambda () (setq firestarter '(recompile)))))

(defvar th/df "/home/thiderman/src/github.com/drunkenfall/drunkenfall/")

;; (defun th/drunkenfall-inotify (event)
;;   (let ((fn (nth 2 event))
;;         (mode (nth 1 event)))
;;     (when (and (equal mode 'changed)
;;                (string= fn (concat th/df "drunkenfall")))
;;       (th/go-server-start "server"))))

;; (file-notify-add-watch th/df '(change attribute-change) 'th/drunkenfall-inotify)

(defhydra th/makefile-hydra (:exit t)
  "Makefile"
  ("c" makefile-executor-execute-last "last target")
  ("f" makefile-executor-goto-makefile "visit file")
  ("C-m" makefile-executor-execute-project-target "project execute")
  ("m" makefile-executor-execute-project-target "project execute"))

(global-set-key (kbd "C-x C-m") 'th/makefile-hydra/body)


(setq go-tc/kwds
      '(("RUN" . font-lock-function-name-face)
        ("PASS" . font-lock-type-face)
        ("FAIL" . font-lock-warning-face)
        ("===\\|---" . font-lock-comment-face)
        ("^WARN .*" . font-lock-warning-face)
        ("20..-..-..T..:..:..Z" . font-lock-comment-face)
        ("20..-..-..T..:..:......\\+...." . font-lock-comment-face)))

(define-minor-mode golang-test-compile-mode
  "Doc string."
  nil "☯" nil
  (font-lock-add-keywords nil go-tc/kwds)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'compilation-mode-hook 'golang-test-compile-mode)

(defun th/compile-current-makefile ()
  (save-excursion
    (beginning-of-buffer)
    (forward-line 3)
    (forward-word 3)
    (thing-at-point 'filename)))

(defun th/compile-goto-makefile ()
  "Edit the Makefile related to the current compilation buffer"
  (interactive)
  (save-excursion
    (find-file-other-window (th/compile-current-makefile))))

(define-key compilation-mode-map (kbd "e") 'th/compile-goto-makefile)

(defun th/compile-execute-target ()
  "Choose a new target in the current makefile"
  (interactive)
  (makefile-executor-execute-target
   (th/compile-current-makefile)))

(define-key compilation-mode-map (kbd "c") 'th/compile-execute-target)

(defun th/compile-execute-dedicated ()
  "Choose a new target in the current makefile and run it in a dedicated buffer."
  (interactive)
  (makefile-executor-execute-dedicated-buffer
   (th/compile-current-makefile)))

(define-key compilation-mode-map (kbd "d") 'th/compile-execute-dedicated)

(provide 'th-compile)
