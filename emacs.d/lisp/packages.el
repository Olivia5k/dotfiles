(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


(require 'package)
(eval-after-load "package"
  '(progn
     (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
     (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
     (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
     (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))))
(package-initialize)
;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)


(defvar elpa-packages
  '(ac-dabbrev
    ace-jump-mode
    ack-and-a-half
    browse-kill-ring
    buffer-move
    crontab-mode
    csv-mode
    dash
    diminish
    discover-my-major
    dockerfile-mode
    epl
    expand-region
    fill-column-indicator
    flx
    flx-ido
    flycheck
    fringe-helper
    gist
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    helm
    helm-projectile
    htmlize
    httpcode
    ido-ubiquitous
    ido-vertical-mode
    ioccur
    js2-mode
    magit
    markdown-mode
    melpa
    morlock
    nginx-mode
    otp
    paradox
    paredit
    pkg-info
    projectile
    puppet-mode
    rainbow-delimiters
    rainbow-identifiers
    rainbow-mode
    rich-minority
    smart-mode-line
    smex
    ssh-config-mode
    yaml-mode
    yasnippet)
  "A list of packages to ensure are installed at launch.")



(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg elpa-packages)
  (when (and (not (package-installed-p pkg))
           (assoc pkg package-archive-contents))
    (package-install pkg)))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `elpa-packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x elpa-packages))
                            (not (package-built-in-p x))
                            (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))


(provide 'packages)
