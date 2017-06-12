(use-package hungry-delete
  :config
  (global-hungry-delete-mode))


(use-package undo-tree
  :bind
  (("C-z"     . undo-tree-undo)
   ("C-x C-z" . undo-tree-undo)
   ("C-M-z"   . undo-tree-redo)
   ("C-x u"   . undo-tree-visualize))

  :config
  (global-undo-tree-mode +1))

(auto-fill-mode 1)
(set-fill-column 79)


(defun th/new-module (fn)
  "Create a new module and add it"
  (interactive "MModule name: ")

  (let ((name (format "'th-%s" fn)))
    (find-file (file-truename
                (expand-file-name "init.el" user-emacs-directory)))
    (end-of-buffer)
    ;; (open-line 1)
    (insert (format "(require %s)" name))
    (save-buffer)

    (find-file (file-truename
                (expand-file-name
                 (format "lisp/th-%s.el" fn)
                 user-emacs-directory)))
    (insert (format "\n\n(provide %s)" name))
    (beginning-of-buffer)))

(global-set-key [f3] #'th/new-module)

(defun th/clear-org ()
  ""
  (interactive)
  (beginning-of-buffer)
  (flush-lines "^\*")
  (flush-lines "#+")
  (delete-blank-lines))

(global-set-key [f6] #'th/clear-org)

(defvar th/clean-whitespace-p t "Cleaning whitespace or not")

(defun th/clean-whitespace ()
  (if th/clean-whitespace-p
      (save-excursion
        (save-restriction
          (delete-trailing-whitespace)
          (widen)
          (goto-char (point-max))
          (delete-blank-lines)))
    (message "Not cleaning whitespace")))

(add-hook 'before-save-hook #'th/clean-whitespace)


(defun th/insert-file-name-to-minibuffer ()
  (interactive)
  (insert (file-truename
           (buffer-name
            (window-buffer (minibuffer-selected-window))))))

(define-key minibuffer-local-map [f3] #'th/insert-file-name-to-minibuffer)


(defun th/comment-block ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "M-;") #'th/comment-block)


(defun th/back-to-indentation-or-bol ()
  "Go to first non whitespace character on a line, or if already on the first
        non whitespace character, go to the beginning of the previous non-blank line."
  (interactive)
  (if (= (point)
         (save-excursion
           (back-to-indentation)
           (point)))
      (beginning-of-line)
    (back-to-indentation)))

(global-set-key (kbd "C-a") #'th/back-to-indentation-or-bol)

(defun th/eol-or-bol ()
  (interactive)
  (if (eolp)
      (back-to-indentation)
    (move-end-of-line nil)))

(global-set-key (kbd "C-e") 'th/eol-or-bol)


(defun th/insertline-and-move-to-line (&optional up)
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

(global-set-key (kbd "C-o") #'th/insertline-and-move-to-line)
(global-set-key (kbd "C-M-o") (lambda ()
                                (interactive)
                                (th/insertline-and-move-to-line t)))


(defun th/kill-line ()
  (interactive)
  (beginning-of-line)
  (if (eq (point) (point-max))
      (previous-line))
  (kill-line 1)
  (back-to-indentation))

(global-set-key (kbd "M-k") #'th/kill-line)

;; The default felt off
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))


(defun th/duplicate-current-line-or-region (arg)
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

(global-set-key (kbd "C-x d") #'th/duplicate-current-line-or-region)


(use-package expand-region
  :bind
  ("M-l"   . er/expand-region)
  ("C-M-l" . er/contract-region))

(use-package multiple-cursors
  :bind
  ("C-x C-l" . mc/mark-next-like-this)
  ("C-x C-h" . mc/mark-previous-like-this)
  ("C-M-s"   . mc/edit-lines))


(defun th/ffap-dwim ()
  "Find the file under point or start browsing for it."
  (interactive)
  (let ((fn (ffap-file-at-point))
        (sym (symbol-name (symbol-at-point))))
    (if fn
        (find-file fn)
      (counsel-find-file sym))))

(global-set-key (kbd "C-x M-f") #'th/ffap-dwim)


;; TODO(thiderman): These kind of need region support
(defun th/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-for-tab-command))

(defun th/move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key [M-up] #'th/move-line-up)
(global-set-key [M-down] #'th/move-line-down)


(use-package rotate-text
  :init
  (setq rotate-text-words '(("width" "height")
                            ("left" "right" "top" "bottom")
                            ("true" "false")
                            ("==" "!=" "<=" ">=")))

  (setq rotate-text-symbols '(("true" "false")
                              ("==" "!=" "<=" ">=")))
  :bind (("M-r" . rotate-text)))


(use-package visual-regexp-steroids
  :bind
  ("C-r" . vr/replace))


;; Pasting into minibuffer
(defun th/paste-from-x-clipboard ()
  (interactive)
  (shell-command "echo $(xsel -o)" 1))

(defun th/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'th/paste-from-x-clipboard)
  (local-set-key [mouse-2] 'th/paste-from-x-clipboard))

(add-hook 'minibuffer-setup-hook 'th/paste-in-minibuffer)

(setq mouse-yank-at-point t)


(defadvice save-buffer (around save-buffer-as-root-around activate)
  "Create non-existing parent directories; sudo to save the current buffer if permissions are lacking."
  (interactive "p")

  ;; Only do any of this if we actually have a file
  (if (buffer-file-name)
      (progn
        ;; Create the parent directories
        (make-directory (f-dirname (buffer-file-name)) t)

        ;; If the file is not writeable, try
        (if (not (file-writable-p (buffer-file-name)))
            (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
              ad-do-it))
        ad-do-it)
    ad-do-it))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(fci-mode -1)
(show-paren-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(electric-pair-mode t)
(global-auto-revert-mode t)
(auto-fill-mode t)

(setq comment-column 42)
(setq fill-column 78)
(setq word-wrap t)

(setq indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq tab-width 2)
(setq standard-indent 2)

(provide 'th-editing)
