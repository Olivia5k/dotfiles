;; Unclutter the interface immediately
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))


;; Start the package configuration
(require 'cl)
(require 'url-handlers)
(setq package-archives
      '(("org"          . "http://orgmode.org/elpa/")
        ("tromey"       . "http://tromey.com/elpa/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Load custom
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; Path configuration for libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; use-package <3
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(setq use-package-always-demand t)

;; ...you know - every once and again... <3
(global-set-key (kbd "<f11>") (lambda () (save-buffers-kill-emacs t)))

;; Libraries that are simply useful always
(use-package no-littering)
(use-package cl-lib)
(use-package dash)
(use-package f)
(use-package s)
(use-package hydra)
(use-package ivy)
(use-package autothemer)

;; Emacs core settings
(require 'th-themes)
(require 'th-window-management)
(require 'th-interface)
(require 'th-modeline)
(require 'th-settings)
(require 'th-backups)
(require 'th-hooks)

;; emacs extensions
(require 'th-company)
(require 'th-dired)
(require 'th-editing)
(require 'th-elisp)
(require 'th-eshell)
(require 'th-fly)
(require 'th-hydra)
(require 'th-snippets)
(require 'th-utilities)

;; Programming and developing
(require 'th-compile)
(require 'th-docker)
(require 'th-env)
(require 'th-git)
(require 'th-manuals)

;; Languages
(require 'th-golang)
(require 'th-python)
(require 'th-web)

;; File browsing and toggling
(require 'th-alternate)
(require 'th-toggle)
(require 'th-quickfast)
(require 'th-context)

;; Organization
(require 'th-email)
(require 'th-elfeed)
(require 'th-org-base)
(require 'th-org-agenda)
