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

(defun th/drunkenfall-inotify (event)
  (let ((fn (nth 2 event))
        (mode (nth 1 event)))
    (when (and (equal mode 'changed)
               (string= fn (concat th/df "drunkenfall")))
      (th/go-server-start "server"))))

(file-notify-add-watch th/df '(change attribute-change) 'th/drunkenfall-inotify)

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
        ("20..-..-..T..:..:..Z" . font-lock-comment-face)))

(define-minor-mode golang-test-compile-mode
  "Doc string."
  nil "blah" nil
  (font-lock-add-keywords nil go-tc/kwds)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'compilation-mode-hook 'golang-test-compile-mode)

(provide 'th-compile)
