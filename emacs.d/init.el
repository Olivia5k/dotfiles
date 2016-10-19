; Nothing is in here. Check out ./emacs.org for the actual configuration.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'url-handlers)
(package-initialize)

(require 'org)
(org-babel-load-file (concat user-emacs-directory "emacs.org"))
(org-babel-load-file (concat user-emacs-directory "share/" "go-mode.org"))
