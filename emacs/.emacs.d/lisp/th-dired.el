(require 'dired)
(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq dired-hide-details-hide-information-lines t)
(setq-default dired-omit-files-p t)
(setq dired-hide-details-mode t)
(setq diredp-hide-details-initially-flag t)
(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|.pyc$\\|.sock$"))
(setq dired-omit-verbose nil) ;; https://open.spotify.com/track/2XRl0NfORYPEvUJXLtJiND
(setq dired-omit-mode t)
(put 'dired-find-alternate-file 'disabled nil)

(bind-key "c" 'wdired-change-to-wdired-mode dired-mode-map)
(bind-key "r" 'rgrep dired-mode-map)
(bind-key "f" 'find-name-dired dired-mode-map)
(bind-key "/" 'th/dired-goto-root dired-mode-map)
(bind-key "~" 'th/dired-goto-home dired-mode-map)
(bind-key "h" 'dired-omit-mode dired-mode-map)
(bind-key "e" 'th/eshell-dired dired-mode-map)

(use-package dired-subtree)
(use-package wdired)

(defun th/dired-goto-root ()
  "Shortcut to browse to root via dired"
  (interactive)
  (dired "/"))

(defun th/dired-goto-home ()
  "Shortcut to browse to $HOME via dired"
  (interactive)
  (dired "~"))

;; Override of the normal find-name-dired since I always want to
;; search in the current directory anyways.
(defun find-name-dired (pattern)
  (interactive "sdired find: ")
  (find-dired
   default-directory
   (concat find-name-arg " " (shell-quote-argument pattern))))

(provide 'th-dired)
