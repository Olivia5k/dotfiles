(use-package compile
  :init
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-auto-jump-to-first-error nil)
  (setq compilation-scroll-output t)
  (setq compilation-read-command nil))


;; Until released for real, this one should be local and not ensured
(use-package makefile+
  :ensure nil
  :load-path "/home/thiderman/git/makefile-plus.el/")

(provide 'th-compile)
