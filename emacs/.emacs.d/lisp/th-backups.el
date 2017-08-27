(use-package real-auto-save
  :init
  (setq real-auto-save-interval 5)
  :config
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (add-hook 'org-mode-hook 'real-auto-save-mode))

(defvar th/backup-directory (concat user-emacs-directory "backups"))

(if (not (file-exists-p th/backup-directory))
    (make-directory th/backup-directory t))

(setq backup-directory-alist `(("." . ,th/backup-directory)))

(setq make-backup-files nil)
(setq backup-by-copying t)
(setq version-control nil)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash nil)
(setq create-lockfiles nil)
(setq kept-old-versions 1)
(setq kept-new-versions 1)

(setq-default auto-save-default nil)
(setq-default auto-save-timeout 5)
(setq-default auto-save-interval 300)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '("."))

(setq backup-inhibited t)
(setq make-backup-files nil)

;; Save all tempfiles in $TMPDIR/emacs-$UID/
(defconst th/emacs-tmp-dir
  (format "/tmp/emacs-%s/" (user-uid)))
(make-directory th/emacs-tmp-dir t)

(setq temporary-file-directory th/emacs-tmp-dir)
(setq auto-save-list-file-prefix th/emacs-tmp-dir)

(provide 'th-backups)
