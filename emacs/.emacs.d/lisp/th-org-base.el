(use-package org
  :bind
  ("s-a" . org-build-agenda)
  (:map org-agenda-mode-map
        ("s-a"   . org-agenda-quit)
        ("f"     . org-agenda-filter-by-category))


  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-directory "~/org")
  (setq org-fontify-emphasized-text t)
  (setq org-return-follows-link t)
  (setq org-hide-leading-stars t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-use-speed-commands t)
  (setq org-use-tag-inheritance nil)
  (setq org-imenu-depth 5)
  (setq org-hierarchical-checkbox-statistics t)
  (setq org-M-RET-may-split-line '((default nil)))
  (setq org-startup-folded nil)
  (setq org-reverse-note-order t)
  (setq org-clock-idle-time 5)


  (setq org-todo-keywords
        '("TODO(t)" "NEXT(n)" "WORKING(w)" "WAITING(z)" "REVIEW(r)" "|" "DONE(d)" "INVALID(i)"))

  (setq org-todo-keyword-faces
        '(("WORKING"  :foreground "#556C21" :weight bold)
          ("NEXT" :foreground "#79740E" :weight bold)
          ("WAITING" :foreground "#3C3246" :weight bold)))

  (defface th/org-agenda-separator
    '((t :foreground "#332033"
         :weight bold))
    "Face for the agenda buffer separators")

  (setq th/org-agenda-faces
        '(("^-\\+$" . th/org-agenda-separator)))

  (defun th/org-agenda-separator-hook ()
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(th/org-agenda-faces)))

  (add-hook 'org-agenda-mode-hook 'th/org-agenda-separator-hook)
  (add-hook 'org-mode-hook 'org-bullets-mode)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sql . t)
     (shell . t)))

  (define-key org-mode-map (kbd "C-c t")   (lambda () (interactive) (org-todo "TODO")))
  (define-key org-mode-map (kbd "C-c n")   (lambda () (interactive) (org-todo "NEXT")))
  (define-key org-mode-map (kbd "C-c w")   (lambda () (interactive) (org-todo "WORKING")))
  (define-key org-mode-map (kbd "C-c z")   (lambda () (interactive) (org-todo "WAITING")))
  (define-key org-mode-map (kbd "C-c r")   (lambda () (interactive) (org-todo "REVIEW")))
  (define-key org-mode-map (kbd "C-c d")   (lambda () (interactive) (org-todo "DONE")))
  (define-key org-mode-map (kbd "C-c i")   (lambda () (interactive) (org-todo "INVALID")))
  (define-key org-mode-map (kbd "C-c SPC") (lambda () (interactive) (org-todo 'none)))

  (define-key org-mode-map (kbd "C-c C-x C-a") 'org-archive-done-tasks)

  (add-hook 'org-clock-in-hook (lambda () (interactive) (org-todo "WORKING")))
  (add-hook 'org-clock-out-hook
          '(lambda ()
             (org-todo "NEXT")
             (setq org-mode-line-string nil)
             (force-mode-line-update))))


(use-package org-journal
  :after org-mode
  :bind (("C-." . org-journal-new-entry))
  :init
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-find-file 'find-file))

(use-package org-habit
  :ensure nil
  :init
  (setq org-modules '(org-habit))
  (setq org-habit-show-habits-only-for-today t))

(use-package org-bullets)

(setq
 org-capture-templates
 `(("t" "Tasks" entry
    (file+headline "~/org/inbox.org" "Inbox")
    "* TODO %^{Task}")

   ("T" "Quick task" entry
    (file+headline "~/org/inbox.org" "Inbox")
    "* TODO %^{Task}\nSCHEDULED: %t\n"
    :immediate-finish t)

   ("i" "Interrupting task" entry
    (file+headline "~/org/inbox.org" "Inbox")
    "* STARTED %^{Task}"
    :clock-in :clock-resume)

   ("e" "Emacs idea" entry
    (file+headline "~/org/inbox.org" "Emacs")
    "* TODO %^{Task}"
    :immediate-finish t)

   ("E" "Event" entry
    (file+datetree+prompt "~/org/events.org" "Event")
    "* TODO %^{Task}\nSCHEDULED: %<%Y-%m-%d %H:%M>"
    :immediate-finish t)

   ("q" "Quick note" item
    (file+headline "~/org/inbox.org" "Quick notes"))

   ("r" "Recipe" entry
    (file+headline "~/org/food.org" "Recipes")
    "* [[%^{URL}][%^{Title}]]"
    )))


(defun th/org-project ()
  "Go to the org project for the current repository.

Go back if we're already in it."

  (interactive)
  (let* ((root (projectile-project-root))
         (name (car (last (s-split "/" (projectile-project-root)) 2))))
    (if (s-equals? (expand-file-name "~/org/") root)
        (progn
          (save-buffer)
          (previous-buffer))
      (find-file
       (format "~/org/%s.org" name)))))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE|INVALID" 'file)
  (save-buffer))

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 5)))

(defun th/org-current-task ()
  "Print the substring of the clocked task, for insertion into a modeline."
  (message (substring-no-properties
            (org-clock-get-clock-string))))

(defhydra th/org (:exit t)
  "Org commands"
  ("C-o" th/org-project "Project file")
  ("c" cfw:open-org-calendar "calendar")
  ("o" org-capture "Capture")
  ("f" (projectile-switch-project-by-name "~/org/") "Files")
  ("s" (org-agenda nil "a") "Schedule")
  ("i" (find-file "~/org/inbox.org") "Inbox")
  ("j" org-clock-goto "Current clocked task")
  ("a" org-todo-list "Agenda")
  ("t" org-tags-view "Tags"))

(global-set-key (kbd "C-x C-o") 'th/org/body)

(use-package worf
  :after org-mode
  :init
  (add-hook 'org-mode-hook 'worf-mode))

(provide 'th-org-base)
