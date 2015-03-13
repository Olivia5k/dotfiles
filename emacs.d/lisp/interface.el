(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x f") 'projectile-find-file) ; Until helm-projectile doesn't crash
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq projectile-completion-system 'helm)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini) ;; I do this way too much...

(require 'projectile)
(projectile-global-mode)

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-z") 'undo)

(require 'fill-column-indicator)
(fci-mode 1)
(setq fci-rule-width 1)
(setq fci-rule-color "#444444")
(add-hook 'after-change-major-mode-hook 'fci-mode)


(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'emacs-lisp-mode-hook 'rainbow-identifiers-mode)

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

(global-set-key (kbd "M-k")
                (lambda ()
                  (interactive)
                  (beginning-of-line)
                  (kill-line 1)
                  (back-to-indentation)))

(defun yank-entire-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark-command)
    (end-of-line)
    (kill-ring-save)))

(defun back-to-indentation-or-beginning ()
  "Go to first non whitespace character on a line, or if already on the first
  non whitespace character, go to the beginning of the line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun move-end-of-line-or-next-line ()
  (interactive)
  (if (eolp)
      (next-line)
    (move-end-of-line nil)))

(defun insertline-and-move-to-line ()
  "Insert an empty line after the current line and positon
  the curson at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1)
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

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
(global-set-key (kbd "C-e") 'move-end-of-line-or-next-line)
(global-set-key (kbd "C-o") 'insertline-and-move-to-line)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(provide 'interface)
