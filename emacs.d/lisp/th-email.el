;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/#using-mu4e-to-send-mail
;; https://gist.github.com/areina/3879626

(use-package mu4e
  :bind
  ("C-x m" . mu4e-hydra/body)
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
  (setq mu4e-marks (remove-nth-element 6 mu4e-marks))
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
  (setq smtpmail-debug-info t)

  (define-key mu4e-headers-mode-map (kbd "i")
    (lambda () (interactive) (mu4e~headers-jump-to-maildir "/gmail/INBOX")))

  (define-key mu4e-headers-mode-map (kbd "TAB") #'th/mu4e-toggle-unread)

  (define-key mu4e-headers-mode-map (kbd "SPC") #'th/mu4e-to-browser)
  (define-key mu4e-view-mode-map (kbd "SPC") #'th/mu4e-to-browser))

(defhydra mu4e-hydra (:exit t)
  "mu4e"
  ("m" (mu4e~headers-jump-to-maildir "/gmail/INBOX") "inbox")
  ("j" mu4e~headers-jump-to-maildir "mailboxes")
  ("c" (mu4e-compose-new) "compose")
  ("u" (mu4e-headers-search "flag:unread AND NOT flag:trashed") "unread"))

;;;###autoload
(defun th/mu4e-to-browser ()
  "Open the current email in a browser window."
  (interactive)
  (shell-command
   (format "eml2browser %s"
           (plist-get (mu4e-message-at-point) :path))))

;;;###autoload
(defun th/mu4e-toggle-unread ()
  "Toggle between showing unread or not."
  (interactive)
  (let* ((query mu4e~headers-last-query)
        (flag " AND flag:unread")
        (final
         (if (s-suffix? flag query)
             (s-chop-suffix flag query)
           (concat mu4e~headers-last-query flag))))
    (mu4e-headers-search final)))

(provide 'th-email)
