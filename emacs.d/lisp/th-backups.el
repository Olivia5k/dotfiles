;; I've never had backsups or autosaving work well in emacs, so these settings
;; are solely here to disable them as much as possible.

(defvar th/backup-directory (concat user-emacs-directory "backups"))

(if (not (file-exists-p th/backup-directory))
    (make-directory th/backup-directory t))

(setq backup-directory-alist `(("." . ,th/backup-directory)))

(setq make-backup-files nil      ; backup of a file the first time it is saved.
      backup-by-copying t        ; don't clobber symlinks
      version-control nil        ; version numbers for backup files
      delete-old-versions t      ; delete excess backup files silently
      delete-by-moving-to-trash nil
      create-lockfiles nil       ;
      kept-old-versions 1        ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 1        ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default nil      ; auto-save every buffer that visits a file
      auto-save-timeout 9000     ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 900000  ; number of keystrokes between auto-saves (default: 300)
      )

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '("."))

(setq backup-inhibited t
      make-backup-files nil)

;; Save all tempfiles in $TMPDIR/emacs-$UID/
(defconst th/emacs-tmp-dir
  (format "/tmp/emacs-%s/" (user-uid)))
(make-directory th/emacs-tmp-dir t)

(setq temporary-file-directory th/emacs-tmp-dir)
(setq auto-save-list-file-prefix th/emacs-tmp-dir)

(provide 'th-backups)
