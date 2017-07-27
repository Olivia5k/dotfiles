;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/#using-mu4e-to-send-mail
;; https://gist.github.com/areina/3879626

(use-package mu4e
  :bind ("C-x m" . mu4e-hydra/body)
  :demand
  :ensure nil

  :init
  (setq mu4e-maildir "~/.mail/")
  (setq mu4e-headers-date-format "%Y-%m-%d")
  (setq mu4e-completing-read-function 'ivy-completing-read)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-confirm-quit nil)

  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))

  ;; Making sure "trashing" is not the same as "deleting"
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target
                                            target) "-N"))))

  ;; I have my "default" parameters from Gmail
  (setq mu4e-sent-folder "/home/thiderman/.mail/sent")
  (setq mu4e-drafts-folder "/home/thiderman/.mail/drafts")
  (setq user-mail-address "lowe.thiderman@gmail.com")
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)

  (require 'smtpmail)

  (setq message-send-mail-function 'smtpmail-send-it)
  (setq starttls-use-gnutls t)
  (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
  (setq smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-debug-info t))

(defhydra mu4e-hydra (:exit t)
  "mu4e"
  ("m" (mu4e~headers-jump-to-maildir "/gmail/INBOX") "inbox")
  ("j" mu4e~headers-jump-to-maildir "mailboxes")
  ("c" (mu4e-compose-new) "compose"))

(provide 'th-email)
