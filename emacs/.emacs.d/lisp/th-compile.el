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

(defhydra th/makefile-hydra (:exit t)
  "Makefile"
  ("c" makefile-executor-execute-last "last target")
  ("f" makefile-executor-goto-makefile "visit file")
  ("C-m" makefile-executor-execute-project-target "project execute")
  ("m" makefile-executor-execute-project-target "project execute"))

(global-set-key (kbd "C-x C-m") 'th/makefile-hydra/body)

(provide 'th-compile)
