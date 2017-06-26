(use-package compile
  :init
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-auto-jump-to-first-error nil)
  (setq compilation-scroll-output t)
  (setq compilation-read-command nil))


(use-package makefile-executor
  :ensure nil
  :demand
  :commands (makefile-executor-execute-target makefile-executor-execute-project-target)
  :load-path "/home/thiderman/git/makefile-executor.el/"
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(provide 'th-compile)
