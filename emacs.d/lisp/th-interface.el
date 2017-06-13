;; avy - directed jumps to words and things
(use-package avy
  :ensure t
  :commands avy-goto-word-1
  :bind
  ("C-l"     . avy-goto-word-1))


;; dumb-jump - IDE-like goto that just uses grep tools and context
(use-package dumb-jump
  :bind (("M-." . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy)
  :ensure)


;; popwin - making most *special-buffers* act like popups
(use-package popwin
  :bind
  ("C-x C-k" . popwin:close-popup-window)
  :bind-keymap
  ("C-x p" . popwin:keymap)

  :config
  (popwin-mode 1)
  ;; It was apparently tricky to make use-package map to what's just a keymap
  (global-set-key (kbd "C-x p") popwin:keymap)

  (setq popwin:special-display-config
        '((help-mode)
          ("*Shell Command Output*")
          (" *undo-tree*" :width 60 :position right))))


;; projectile - project management <3
(use-package projectile
  :bind (("C-x f" . projectile-find-file))
  :bind-keymap (("C-x p" . projectile-command-map))
  :commands (projectile-switch-project)
  :demand

  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy
        projectile-mode-line nil))


;; ivy - better fuzzy selection
(use-package ivy
  :bind (("M-x"     . counsel-M-x)
         ("C-x y"   . counsel-yank-pop)
         ("C-c C-r" . ivy-resume))
  :commands (ivy-set-actions)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20)
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
            (find-file fn))
      "horz")
     ("s" (lambda (fn)
            (interactive)
            (split-window-right)
            (windmove-right)
            (find-file fn))
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
      "delete")))

  (ivy-mode 1))

(use-package counsel
  :bind (("C-x M-p" . counsel-package))
  :config
  ;; https://github.com/abo-abo/swiper/issues/685#issuecomment-249162962
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))


(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))


(defhydra th/search-hydra (:exit t :foreign-keys warn)
  "Searching"
  ("C-s" counsel-grep-or-swiper "search")
  ("s" counsel-grep-or-swiper "search")
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


(use-package rainbow-mode)
(use-package rainbow-delimiters)
(use-package rainbow-identifiers)


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



;; linum is disabled
(global-linum-mode 0)
(setq linum-format " %4d ")

;; Indicators on the edges of the screen
(fringe-mode 12)
(setq-default indicate-empty-lines t)

(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.2)


(provide 'th-interface)
