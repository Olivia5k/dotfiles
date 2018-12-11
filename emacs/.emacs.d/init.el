;; Unclutter the interface immediately
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))

;; This has caused me enough pain now.
(global-unset-key (kbd "C-z"))

;; Disable mouse focus for exwm
(setq mouse-autoselect-window nil
      focus-follows-mouse nil)

;; Start the package configuration
(require 'cl)
(require 'url-handlers)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Initialize straight
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq use-package-version 'straight)

;; Load custom
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; Path configuration for libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Emacs core settings
(require 'th-core)
(require 'th-themes)
(require 'th-git)
(require 'th-window-management)
(require 'th-interface)
(require 'th-modeline)
(require 'th-backups)
(require 'th-hooks)
(require 'th-utilities)
(require 'th-toggle)

;; emacs extensions
(require 'th-company)
(require 'th-dired)
(require 'th-editing)
(require 'th-elisp)
(require 'th-eshell)
(require 'th-fly)
(require 'th-hydra)
(require 'th-snippets)
(require 'th-ssh)
(require 'th-work)
(require 'th-abbrev)

;; Programming and developing
(require 'th-compile)
(require 'th-docker)
(require 'th-enved)
(require 'th-lisp)

;; Languages
(require 'th-golang)
(require 'th-python)
(require 'th-web)

;; File browsing and toggling
(require 'th-alternate)
(require 'th-quickfast)

;; Organization
(require 'th-email)
(require 'th-org-base)
(require 'th-chat)
(require 'th-exwm)
(require 'exwm-status)

;; Media
(require 'th-spotify)

;; Lastly
(require 'th-last)
(require 'th-exwm-workspace)
