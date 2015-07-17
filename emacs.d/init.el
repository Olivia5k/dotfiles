;; init.el - where it all begins

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq custom-theme-directory (concat user-emacs-directory "themes"))

;;; Package configuration
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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
  '(ace-jump-mode
    ack-and-a-half
    anaconda-mode
    browse-kill-ring
    buffer-move
    company
    company-go
    crontab-mode
    csv-mode
    dash
    diminish
    discover-my-major
    dockerfile-mode
    emmet
    epl
    expand-region
    fill-column-indicator
    flx
    flx-ido
    flycheck
    fringe-helper
    gist
    git-commit-mode
    git-messenger
    git-timemachine
    gitconfig-mode
    gitignore-mode
    go-mode
    helm
    helm-ag
    helm-c-yasnippet
    helm-descbinds
    helm-projectile
    helm-swoop
    htmlize
    httpcode
    ido-ubiquitous
    ido-vertical-mode
    ioccur
    js2-mode
    lispy
    magit
    magit-gh-pulls
    markdown-mode
    melpa
    multiple-cursors
    nginx-mode
    otp
    paradox
    paredit
    pkg-info
    popwin
    projectile
    puppet-mode
    rainbow-delimiters
    rainbow-identifiers
    rainbow-mode
    rich-minority
    s
    smart-mode-line
    ssh-config-mode
    string-inf
    virtualenvwrapper
    wrap-region
    xkcd
    yaml-mode
    yasnippet
    zenburn)
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


;;; Options and settings
(require 's)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indicate-empty-lines t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(mouse-wheel-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

(setq backup-inhibited t
      auto-save-default nil)

(setq backup-by-copying t)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (format "/tmp/emacs-%s/" (user-uid)))
(make-directory emacs-tmp-dir t)
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq temporary-file-directory emacs-tmp-dir)

(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(setq auto-save-interval 0
      auto-save-timeout 1)

(setq echo-keystrokes 0.4
      stack-trace-on-error nil
      standard-indent 4
      tab-always-indent 'complete
      grep-scroll-output t)

(setq-default comment-column 42
              fill-column 78
              indent-tabs-mode nil
              tab-width 2
              word-wrap t)

(show-paren-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)
(auto-fill-mode t)

;;; popwin
(require 'popwin)
(popwin-mode 1)

;;; Helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x f") 'helm-projectile)
(global-set-key (kbd "M-q") 'helm-mini)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

;; (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
;; (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
;; (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

(defun helm-toggle-header-line ()
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-source-header-default-foreground
                          :background helm-source-header-default-background
                          :box helm-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))

;; helm-swoop
(require 'helm-swoop)

(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; disable pre-input
(setq helm-swoop-pre-input-function
      (lambda () ""))

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

;;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-mode-line nil)

;;; yas
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t) ;[default: nil]
(global-set-key (kbd "C-.") 'helm-yas-complete)
(yas-global-mode 1)

;;; Autocompletion
(require 'company)
(require 'company-go)

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;;; golang
(require 'go-mode)

(defun th-go-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)

  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c C-i") 'go-remove-unused-imports))

(add-hook 'go-mode-hook 'th-go-hook)

;;; Smart mode line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/apply-theme 'darktooth)
(sml/setup)

;;; undo tree
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-x C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)
(global-undo-tree-mode +1)

;;; FCI
(require 'fill-column-indicator)
(fci-mode 1)
(setq fci-rule-width 1)
(setq fci-rule-color "#404049")
(add-hook 'after-change-major-mode-hook 'fci-mode)

(auto-fill-mode 1)
(set-fill-column 79)

;;; ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; emmet
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook
          (lambda ()
            (setq emmet-indentation 2))) ;; indent 2 spaces.

;;; Rainbows and docs <3
(add-hook 'emacs-lisp-mode-hook 'rainbow-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; Editor macros
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
   If there's no region, the current line will be duplicated. However, if
   there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-x d") 'duplicate-current-line-or-region) ; fak u paredit <3

(global-set-key (kbd "M-k")
                (lambda ()
                  (interactive)
                  (beginning-of-line)
                  (if (eq (point) (point-max))
                      (previous-line))
                  (kill-line 1)
                  (back-to-indentation)))

(defun yank-entire-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark-command)
    (end-of-line)
    (kill-ring-save)))

(defun back-to-indentation-or-previous-line ()
  "Go to first non whitespace character on a line, or if already on the first
  non whitespace character, go to the beginning of the previous non-blank line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (previous-line))
  (if (and (eolp) (bolp))
      (back-to-indentation-or-previous-line))
  (back-to-indentation))

(defun move-end-of-line-or-next-line ()
  (interactive)
  (if (eolp)
      (progn
        (next-line)
        (if (bolp)
            (move-end-of-line-or-next-line))))
  (move-end-of-line nil))

(defun insertline-and-move-to-line (&optional up)
  "Insert a newline, either below or above depending on `up`. Indent accordingly."
  (interactive)
  (beginning-of-line)
  (if up
      (progn
        (newline)
        (forward-line -1))
    (move-end-of-line nil)
    (open-line 1)
    (forward-line 1))
  (indent-according-to-mode))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-a") 'back-to-indentation-or-previous-line)
(global-set-key (kbd "C-e") 'move-end-of-line-or-next-line)
(global-set-key (kbd "C-o") 'insertline-and-move-to-line)
(global-set-key (kbd "C-M-o") (lambda ()
                                (interactive)
                                (insertline-and-move-to-line t)))

(defun forward-word-to-beginning (&optional n)
  "Move point forward n words and place cursor at the beginning."
  (interactive "p")
  (let (myword)
    (setq myword
      (if (and transient-mark-mode mark-active)
        (buffer-substring-no-properties (region-beginning) (region-end))
        (thing-at-point 'symbol)))
    (if (not (eq myword nil))
      (forward-word n))
    (forward-word n)
    (backward-word n)))

(global-set-key (kbd "M-f") 'forward-word-to-beginning)

(global-set-key (kbd "C-r") 'vr/replace)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;;; magit
(require 'magit)
(global-set-key (kbd "M-g") 'magit-status)

(setq magit-save-some-buffers 'dontask)
(setq magit-last-seen-setup-instructions "1.4.0")

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  "Restores the previous window configuration and kills the magit buffer"
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-mode-quit-window)

;;; Python
(require 'python)
(require 'snakecharmer)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(define-key python-mode-map (kbd "C-c C-d") 'anaconda-mode-goto-definitions)

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'flycheck-first-error)))

;;; Flycheck
(require 'flycheck)
(global-set-key (kbd "C-x C-n") 'flycheck-next-error)
(global-set-key (kbd "C-x C-p") 'flycheck-previous-error)
(global-set-key (kbd "C-c C-SPC")
                (lambda ()
                  (interactive)
                  (let ((name "*Flycheck errors*"))
                    (if (get-buffer name)
                        (progn
                          (kill-buffer name)
                          (message "Killed buffer"))
                      (flycheck-list-errors)))))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; Elisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'lispy-mode)

;;; Misc options
(defun eval-buffer-or-region (point mark)
  (interactive "r")
  (if (region-active-p)
      (progn
        (eval-region point mark)
        (keyboard-escape-quit) ;; Is it possible to quit region otherwise?
        (message "Region eval:ed"))
    (eval-buffer)))

(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer-or-region)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "<f11>") 'save-buffers-kill-emacs)

(setq debug-on-error nil)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'er/contract-region)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;; ido settings
(setq ido-create-new-buffer 'always
      ido-default-buffer-method 'selected-window
      ido-case-fold t
      ido-enable-last-directory-history nil
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-enable-flex-matching t
      ido-max-prospects 15
      ido-confirm-unique-completion t
      ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                        " [No match]" " [Matched]" " [Not readable]"
                        " [Too big]" " [Confirm]"))

(require 'ido)
;; (ido-mode t)
;; (ido-everywhere t)

;; pls no .elc
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'after-save-hook)
            (add-hook 'after-save-hook
                      (lambda ()
                        (if (file-exists-p (concat buffer-file-name "c"))
                            (delete-file (concat buffer-file-name "c")))))))

;;; <f> bindings

;; discover-my-major pls
(define-key global-map (kbd "<f1>")
  (lambda ()
    (interactive)
    (if (s-prefix? "*makey-key" (buffer-name))
        (kill-buffer-and-window)
      (discover-my-major t))))

;; Go to a certain file. If already in it, go back.
(defun th-toggle-file (path)
 (let ((file (file-truename path)))
   (if (s-equals? file buffer-file-name)
       (progn
         (save-buffer)
         (previous-buffer))
     (find-file file))))

;; init.el is love, init.el is life
(define-key global-map (kbd "<f2>")
  (lambda ()
    (interactive)
    (th-toggle-file (concat user-emacs-directory "init.el"))))

;; Org mode switch!
(define-key global-map (kbd "<f3>")
  (lambda ()
    (interactive)
    (th-toggle-file (concat user-emacs-directory "todo.org"))))

;; Scratch buffer go
(define-key global-map (kbd "<f4>")
  (lambda ()
    (interactive)
    (let ((content initial-scratch-message)
          (buf "*scratch*"))
      (when (get-buffer buf)
        (setq content ""))
      (switch-to-buffer buf)
      (insert content))))

;;; Backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 2               ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 20            ; number of keystrokes between auto-saves (default: 300)
      )

;;; Save hooks

(add-hook 'before-save-hook
          (lambda ()
            (save-excursion
              (save-restriction
                (delete-trailing-whitespace)
                (widen)
                (goto-char (point-max))
                (delete-blank-lines)))))

;;; Ace jump
(define-key global-map (kbd "C-;") 'ace-jump-word-mode)

;;; Kill ring
(define-key global-map (kbd "C-x y") 'browse-kill-ring)

;;; Window management
(defun th-split-window (vertical &optional helming)
  "Split a window and go to it, optionally open helm-mini."
  (if vertical
      (progn
        (split-window-vertically)
        (windmove-down))
    (progn
      (split-window-horizontally)
      (windmove-right)))
  (if helming
      (helm-projectile)))

(define-key global-map (kbd "M-0") 'delete-window)
(define-key global-map (kbd "M-1") 'delete-other-windows)
(define-key global-map (kbd "M-2") (lambda ()
                                     (interactive)
                                     (th-split-window t t)))
(define-key global-map (kbd "M-3") (lambda ()
                                     (interactive)
                                     (th-split-window nil t)))
(define-key global-map (kbd "M-4") 'kill-buffer-and-window)
(define-key global-map (kbd "M-=") 'balance-windows)

(define-key global-map (kbd "C-M-2") (lambda ()
                                       (interactive)
                                       (th-split-window t)))
(define-key global-map (kbd "C-M-3") (lambda ()
                                       (interactive)
                                       (th-split-window nil)))

(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x l") 'windmove-right)

(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

(global-set-key (kbd "s-M-h") 'buf-move-left)
(global-set-key (kbd "s-M-j") 'buf-move-down)
(global-set-key (kbd "s-M-k") 'buf-move-up)
(global-set-key (kbd "s-M-l") 'buf-move-right)

(global-set-key (kbd "s-f") 'delete-other-windows)
(global-set-key (kbd "C-q") 'delete-window)

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;;; Appearances
(set-default-font "Inconsolata-14")
(load-theme 'darktooth t)

(global-linum-mode 1)
(setq linum-format " %3d ")
(fringe-mode 12)
(setq scroll-step 10)

(setq tty-color-mode 256)

;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
(defvar mode-line-cleaner-alist
  `((paredit-mode . " ()")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (auto-fill-mode "")
    (helm-mode "")
    (magit-auto-revert-mode "")
    (undo-tree-mode " ⎌")

    ;; Major modes
    (help-mode . "")
    (fundamental-mode . "0")
    (python-mode . "py")
    (emacs-lisp-mode . "el")))

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; why custom why
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
