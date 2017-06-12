(use-package compile
  :bind (:map compilation-mode-map
              ("q" . th/quit-compilation-buffer)
              ;; `C-c l` is because that same command is used in golang and others
              ("C-c l" . th/toggle-maximize-buffer)

              :map go-mode-map
              ("C-c l" . th/maximize-log))

  :init
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-auto-jump-to-first-error nil
        compilation-scroll-output t
        compilation-read-command nil)

  (defun th/toggle-maximize-buffer (&optional buffer-name)
    "Maximize buffer"
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (if buffer-name
            (switch-to-buffer buffer-name))
        (delete-other-windows))))

  (defun th/maximize-log ()
    (interactive)
    (th/toggle-maximize-buffer "*compilation*"))

  (defun th/quit-compilation-buffer ()
    ;; TODO: Maybe this can be done with advice instead?
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (quit-window))))


;; Until released for real, this one should be local and not ensured
(use-package makefile+
  :ensure nil
  :load-path "/home/thiderman/git/makefile-plus.el/")

(provide 'th-compile)
