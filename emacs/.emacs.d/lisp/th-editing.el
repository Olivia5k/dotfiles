(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))


(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "e"))


(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (("C-z"     . undo-tree-undo)
   ("C-x C-z" . undo-tree-undo)
   ("C-M-z"   . undo-tree-redo)
   ("C-x u"   . undo-tree-visualize))

  :config
  (global-undo-tree-mode +1))

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

(defvar th/clean-whitespace-p t "Cleaning whitespace or not")

(setq th/clean-whitespace-p t)

(defun th/clean-whitespace ()
  (if (and th/clean-whitespace-p
           (not (looking-back " ")))
      (save-excursion
        (save-restriction
          (delete-trailing-whitespace)
          (widen)
          (goto-char (point-max))
          (delete-blank-lines)))
    (message "Not cleaning whitespace")))

(add-hook 'before-save-hook #'th/clean-whitespace)

(setq completion-styles '(initials basic partial-completion emacs22))

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

(global-set-key (kbd "M-k") (lambda () (interactive) (message "key is disabled")))

;; The default felt off
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))


;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(defun th/copy-unfilled-paragraphs ()
  "Kills paragraphs as one line, suitable for pasting to things that don't understand Markdown."
  (interactive)
  (when (not (region-active-p))
    (error "No region active; cannot unfill paragraphs"))

  (kill-ring-save 0 0 t)
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (push-mark (point-max))
    (setq mark-active t)
    (unfill-paragraph t)
    (kill-ring-save (point-min) (point-max))
    (message "Unfilled paragraphs killed")))

(global-set-key (kbd "C-M-w") 'th/copy-unfilled-paragraphs)

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
  ("C-M-l" . er/contract-region)
  ("C-c SPC" . hydra-mark/body)
  ("C-c C-SPC" . hydra-mark/body)

  :config
  (setq shift-select-mode nil) ;; https://github.com/magnars/expand-region.el/issues/220
  (use-package change-inner)

  (defhydra hydra-mark (:color blue :columns 4)
    "Mark / mc"
    ("a" mc/mark-all-dwim "mark all")
    ("l" mc/mark-next-like-this "mark next" :exit nil)
    ("C-SPC" mc/mark-next-like-this "mark next" :exit nil)
    ("h" mc/mark-previous-like-this "mark previous" :exit nil)
    ("e" mc/edit-lines "edit lines")

    ("d" er/mark-defun "Defun / Function")
    ("f" er/mark-defun "Defun / Function")
    ("w" er/mark-word "Word")
    ("u" er/mark-url "Url")
    ("E" er/mark-email "Email")
    ("b" mark-whole-buffer "Buffer")
    ("p" er/mark-text-paragraph "Paragraph")
    ("s" er/mark-symbol "Symbol")
    ("S" er/mark-symbol-with-prefix "Prefixed symbol")
    ("q" er/mark-inside-quotes "Inside Quotes")
    ("Q" er/mark-outside-quotes "Outside Quotes")
    ("(" er/mark-inside-pairs "Inside Pairs")
    ("[" er/mark-inside-pairs "Inside Pairs")
    ("{" er/mark-inside-pairs "Inside Pairs")
    (")" er/mark-outside-pairs "Outside Pairs")
    ("]" er/mark-outside-pairs "Outside Pairs")
    ("}" er/mark-outside-pairs "Outside Pairs")
    ("t" er/mark-inner-tag "Inner Tag")
    ("T" er/mark-outer-tag "Outer Tag")
    ("c" er/mark-comment "Comment")
    ("i" change-inner "Inner")
    ("o" change-outer "Outer")
    ("SPC" er/expand-region "Expand Region" :exit nil)
    ("M-SPC" er/contract-region "Contract Region" :exit nil)
    ("q" nil)))

(use-package multiple-cursors)


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



(use-package visual-regexp-steroids
  :bind
  ("C-r" . vr/replace))


(use-package hexrgb
  :commands (hexrgb-increment-red hexrgb-increment-green hexrgb-increment-blue))

(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode 1))

;; Hydra that can be used to gradually increase or decrease hex
;; colors. Very useful when designing color themes!
(defhydra th/hexrgb-hydra (:foreign-keys warn)
  "
Colors (rgb+, rgb-) [%`th/hexrgb-step]
"
  ("q" (th/hexrgb #'hexrgb-increment-red 1))
  ("w" (th/hexrgb #'hexrgb-increment-green 1))
  ("e" (th/hexrgb #'hexrgb-increment-blue 1))
  ("a" (th/hexrgb #'hexrgb-increment-red -1))
  ("s" (th/hexrgb #'hexrgb-increment-green -1))
  ("d" (th/hexrgb #'hexrgb-increment-blue -1))
  ("i" (setq th/hexrgb-step (read-number "Step number: ")) "Set increment")
  ("x" nil "exit" :exit t))

(defvar th/hexrgb-step 1 "Steps to move when using th/hexrgb")

(defun th/hexrgb (fun val)
  "Helper that runs color increments via the `th/hexrgb-hydra'."
  (when (looking-at "#")
    (forward-char 1))
  (let* ((color (format "#%s" (word-at-point)))
         (result (funcall fun color 2 (* th/hexrgb-step val))))
    (when (not (looking-at "#"))
      (search-backward "#"))
    (delete-char 1)
    (kill-word 1)
    (insert result)))


(defun th/filter-symbol-at-point (p)
  "Use `keep-lines' or `flush-lines' on the symbol under the cursor.

For the latter, give the prefix argument."
  (interactive "P")
  (save-excursion
    (let* ((name (symbol-name (symbol-at-point)))
           (func (if p 'flush-lines 'keep-lines)))
      (goto-char (point-min))
      (funcall func name)
      (message "filter-symbol-at-point: (%s \"%s\")" func name))))

(global-set-key (kbd "C-x C-i") 'th/filter-symbol-at-point)

;; Pasting into minibuffer
(defun th/paste-from-x-clipboard ()
  (interactive)
  (shell-command "echo $(xsel -o)" 1))

(defun th/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'th/paste-from-x-clipboard)
  (local-set-key [mouse-2] 'th/paste-from-x-clipboard))

(add-hook 'minibuffer-setup-hook 'th/paste-in-minibuffer)

(setq mouse-yank-at-point t)


(defun crontab-e ()
  (interactive)
  (with-editor-async-shell-command "crontab -e"))


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

(show-paren-mode t)

(global-set-key (kbd "C-x l") 'downcase-region)

(global-set-key (kbd "<C-tab>") 'dabbrev-expand)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(electric-pair-mode t)
(global-auto-revert-mode t)
(auto-fill-mode t)

(setq comment-column 42)
(setq fill-column 78)
(setq word-wrap t)
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(provide 'th-editing)
