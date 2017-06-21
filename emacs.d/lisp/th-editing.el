(use-package hungry-delete
  :demand
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
  ("C-M-l" . er/contract-region)
  ("C-c SPC" . hydra-mark/body)

  :config
  (setq shift-select-mode nil) ;; https://github.com/magnars/expand-region.el/issues/220
  (use-package change-inner)

  (defhydra hydra-mark (:color blue :idle 1.5 :columns 4)
    "Mark"
    ("d" er/mark-defun "Defun / Function")
    ("f" er/mark-defun "Defun / Function")
    ("w" er/mark-word "Word")
    ("u" er/mark-url "Url")
    ("e" mark-sexp "S-Expression")
    ("E" er/mark-email "Email")
    ("b" mark-whole-buffer "Buffer")
    ("l" mark-line "Line")
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
    ("a" er/mark-html-attribute "HTML Attribute")
    ("i" change-inner "Inner")
    ("o" change-outer "Outer")
    ("." er/expand-region "Expand Region" :exit nil)
    ("," er/contract-region "Contract Region" :exit nil)))

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


(use-package hexrgb
  :commands (hexrgb-increment-red hexrgb-increment-green hexrgb-increment-blue))

;; Hydra that can be used to gradually increase or decrease hex
;; colors. Very useful when designing color themes!
;; TODO(thiderman): Having the th/hexrgb-step variable in the hydra docstring
(defhydra th/hexrgb-hydra (:foreign-keys warn)
  "Colors (rgb+, rgb-)"
  ("q" (th/hexrgb #'hexrgb-increment-red 1))
  ("w" (th/hexrgb #'hexrgb-increment-green 1))
  ("e" (th/hexrgb #'hexrgb-increment-blue 1))
  ("a" (th/hexrgb #'hexrgb-increment-red -1))
  ("s" (th/hexrgb #'hexrgb-increment-green -1))
  ("d" (th/hexrgb #'hexrgb-increment-blue -1))
  ("i" (setq th/hexrgb-step (read-number "Step number: ")) "Set increment")
  ("x" nil "exit" :exit t))

(defvar th/hexrgb-step 1 "Steps to move when using th/hexrgb-hydra")

(defun th/hexrgb (fun val)
  "Helper that runs color increments via the `th/hexrgb-hydra'."
  (when (looking-at "#")
    (forward-char 1))
  (let* ((color (format "#%s" (word-at-point)))
         (result (funcall fun color 2 (* th/hexrgb-step val))))
    (search-backward "\"")
    (forward-char 1)
    (kill-word 1)
    (insert result)))


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

(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(provide 'th-editing)
