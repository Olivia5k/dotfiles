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

;; Path configuration for libraries and themes
(defvar emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(add-to-list 'load-path (expand-file-name "lisp" emacs-d))

(setq custom-theme-directory (concat user-emacs-directory "themes"))
(dolist (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; use-package <3
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

;; ...you know - every once and again... <3
(global-set-key (kbd "<f11>") 'save-buffers-kill-emacs)

;; Libraries that are simply useful always
(use-package cl-lib)
(use-package dash)
(use-package f)
(use-package s)
(use-package hydra)
(use-package ivy)

;; Loading of all the modules
(require 'th-themes)
(require 'th-interface)
(require 'th-company)
(require 'th-editing)
(require 'th-snippets)
(require 'th-compile)
(require 'th-elfeed)
(require 'th-elisp)
(require 'th-org)
(require 'th-utilities)
(require 'th-web)
(require 'th-python)
(require 'th-hooks)
(require 'th-alternate)
(require 'th-toggle)
(require 'th-context)
(require 'th-docker)
(require 'th-env)
(require 'th-dired)
(require 'th-fly)
(require 'th-git)
(require 'th-hydra)
(require 'th-backups)
(require 'th-settings)
(require 'th-eshell)
(require 'th-modeline)
(require 'th-window-management)
