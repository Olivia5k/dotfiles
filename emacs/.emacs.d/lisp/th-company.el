(use-package company
  :diminish company-mode
  :config
  (use-package company-flx
    :config
    (company-flx-mode +1))

  ;; Let company do its thing as often as possible.
  (global-company-mode t)

  (setq company-tooltip-limit 20)       ; bigger popup window
  (setq company-idle-delay .3) ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)  ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

  ;; https://oremacs.com/2017/12/27/company-numbers/
  (setq company-show-numbers t)
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))

(use-package company-go
  :after company)

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

(provide 'th-company)
