(use-package org
  :bind (:map org-mode-map
              ("C-c ;" . org-edit-special)
              :map org-src-mode-map
              ("C-c ;" . org-edit-src-exit))

  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-directory "~/org")
  (setq org-fontify-emphasized-text t)
  (setq org-hide-leading-stars t)
  (setq org-return-follows-link t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-use-speed-commands t)
  (setq org-imenu-depth 5)
  (setq org-hierarchical-checkbox-statistics t)

  (add-hook 'org-mode-hook 'org-bullets-mode)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sql . t)
     (shell . t)
     (python . t)
     (js . t)))

  (define-key org-mode-map (kbd "C-c w")   (lambda () (interactive) (org-todo "WORKING")))
  (define-key org-mode-map (kbd "C-c z")   (lambda () (interactive) (org-todo "WAITING")))
  (define-key org-mode-map (kbd "C-c r")   (lambda () (interactive) (org-todo "REVIEW")))
  (define-key org-mode-map (kbd "C-c d")   (lambda () (interactive) (org-todo "DONE")))
  (define-key org-mode-map (kbd "C-c i")   (lambda () (interactive) (org-todo "INVALID")))
  (define-key org-mode-map (kbd "C-c SPC") (lambda () (interactive) (org-todo 'none)))

  (define-key org-mode-map (kbd "C-c C-x C-a") 'org-archive-done-tasks))


(use-package org-journal
  :config
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-find-file 'find-file))


(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-ndays 7)
(setq org-agenda-files '("~/org/"))
(setq org-agenda-show-all-dates t)
(setq org-agenda-start-on-weekday nil)
(setq org-archive-location "~/org/archive/%s::")
(setq org-log-done t) ;;timestamp when switching from todo to done

(setq org-todo-keywords
      '("TODO(t)" "WORKING(w)" "WAITING(z)" "REVIEW(r)" "|" "DONE(d)" "INVALID(i)"))

(setq org-todo-keyword-faces '(("WORKING" . org-scheduled-today)
                               ("WAITING" . org-mode-line-clock)))


;; Since I am planning to do a lot of these, let's just pick a super simple keybind!
(global-set-key (kbd "C-.") 'org-journal-new-entry)
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
   "/DONE" 'file))


(setq org-refile-targets
      '((org-agenda-files :maxlevel . 5)
        ("~/git/dotfiles/emacs.d/emacs.org" :level . 2)))

(defhydra th/org (:exit t)
  "Org commands"
  ("C-o" th/org-project "Project file")
  ("c" org-capture "Capture")
  ("o" org-capture "Capture")
  ("f" (projectile-switch-project-by-name "~/org/") "Files")
  ("s" (org-agenda nil "a") "Schedule")
  ("i" (find-file "~/org/inbox.org") "Inbox")
  ("j" org-clock-goto "Current clocked task")
  ("a" org-todo-list "Agenda")
  ("t" org-tags-view "Tags"))

(global-set-key (kbd "C-x C-o") 'th/org/body)


(use-package worf
  :init
  (add-hook 'org-mode-hook 'worf-mode))

(provide 'th-org)
