;; avy - directed jumps to words and things
(use-package avy
  :ensure t
  :commands avy-goto-word-1
  :bind
  ("C-l"     . avy-goto-word-1))


;; dumb-jump - IDE-like goto that just uses grep tools and context
(use-package dumb-jump
  :bind (("M-." . dumb-jump-go)
         ("C-x ." . th/dumb-jump-hydra/body))
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'ag)

  (defhydra th/dumb-jump-hydra (:exit t :foreign-keys warn)
    "Dumb Jump"
    ("g" dumb-jump-go "Go")
    ("b" dumb-jump-back "Back")
    ("l" dumb-jump-quick-look "Look")
    ("e" dumb-jump-go-prefer-external "External")
    ("w" dumb-jump-go-other-window "Window")
    ("q" nil)))


;; projectile - project management <3
(use-package projectile
  :bind-keymap (("C-x p" . projectile-command-map))
  :commands (projectile-switch-project)

  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-files-cache-expire (* 24 60 60))
  (setq projectile-mode-line nil))


;; ivy - better fuzzy selection
(use-package ivy
  :diminish ivy-mode
  :bind (("M-x"     . counsel-M-x)
         ("C-x y"   . counsel-yank-pop)
         ("C-c C-r" . ivy-resume))
  :commands (ivy-set-actions)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 25)
  (setq magit-completing-read-function 'ivy-completing-read)

  ;; So that we can do space separation in file name completions
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  (ivy-set-actions
   'projectile-find-file
   '(("z" (lambda (fn)
            (interactive)
            (split-window-below)
            (windmove-down)
            (find-file (expand-file-name fn (projectile-project-root))))
      "horz")
     ("s" (lambda (fn)
            (interactive)
            (split-window-right)
            (windmove-right)
            (find-file (expand-file-name fn (projectile-project-root))))
      "vert")))

  (ivy-set-actions
   'ivy-switch-buffer
   '(("z" (lambda (buf)
            (interactive)
            (split-window-below)
            (windmove-down)
            (switch-to-buffer buf))
      "horz")

     ("s" (lambda (buf)
            (interactive)
            (split-window-right)
            (windmove-right)
            (switch-to-buffer buf))
      "vert")

     ("d" (lambda (buf)
            (interactive)
            (kill-buffer buf)
            (message "Buffer %s killed" buf))
      "delete")
     ("k"
      (lambda (x)
        (kill-buffer x)
        (ivy--reset-state ivy-last))
      "kill")
     ("j"
      ivy--switch-buffer-other-window-action
      "other window")
     ("r"
      ivy--rename-buffer-action
      "rename")))

  (ivy-mode 1))

(use-package counsel
  :bind (("C-x C-f" . counsel-find-file))
  :config
  ;; https://github.com/abo-abo/swiper/issues/685#issuecomment-249162962
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))

(use-package counsel-projectile
  :bind (("C-x f" . counsel-projectile-find-file))
  :after counsel)


(use-package ace-link
  :config
  (ace-link-setup-default))


(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))


(defhydra th/search-hydra (:exit t :foreign-keys warn)
  "Searching"
  ("C-s" counsel-grep-or-swiper "search")
  ("s" counsel-grep-or-swiper "search")
  ("d" (rgrep (read-string "dir search: ") "*" default-directory) "dir")
  ("a" swiper-all "all")
  ("g" counsel-git-grep "git grep")
  ("i" counsel-imenu "imenu")
  ("k" counsel-descbinds "keys"))

(global-set-key (kbd "C-s") 'th/search-hydra/body)


(use-package fixme-mode
  :config
  (add-hook 'prog-mode-hook 'fixme-mode)
  (setq fixme-mode-warning-words
        '("FIXME" "TODO" "BUG" "KLUDGE" "FIX" "FixMe" "HACK"
          "REFACTOR" "NOCOMMIT" "XXX")))


(use-package helpful
  :bind
  ("C-h f" . helpful-function)
  ("C-c h" . hydra-helpful/body)

  :config
  (defhydra hydra-helpful (:color blue)
    "Helpful"
    ("f" helpful-function "Function")
    ("c" helpful-command "Command")
    ("m" helpful-macro "Macro")))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun th/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'th/narrow-or-widen-dwim)


(global-set-key (kbd "C-x <C-return>") 'browse-url)

;; Indicators on the edges of the screen
(fringe-mode 12)
(setq-default indicate-empty-lines t)
(setq-default cursor-in-non-selected-windows nil)

(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.2)


(provide 'th-interface)
