(use-package org
  :init
  (require 'org)
  (require 'org-agenda)
  (require 'org-macs)

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

  (setq org-refile-targets
        '(("~/org/gtd.org" :maxlevel . 3)
          ("~/org/someday.org" :level . 1)
          ("~/org/tickler.org" :maxlevel . 2)))

  (setq
   org-capture-templates
   `(("t" "Todo [inbox]" entry
      (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %i%?")

     ("T" "Tickler" entry
      (file+headline "~/org/tickler.org" "Tickler")
      "* %i%? \n %U")

     ("w" "Track weight" entry
      (file+headline "~/org/irl.org" "Weight")
      "* %T %^{Current weight} kg"
      :immediate-finish t)

     ("l" "Line" entry
      (file "~/org/code.org")
      "* [[file://%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%^{Description}]]"
      )))

  (setq org-agenda-ndays 7)
  (setq org-agenda-files '("~/org/inbox.org"
                           "~/org/gtd.org"
                           "~/org/tickler.org"))
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-block-separator ?-)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-window-setup 'current-window)
  (setq org-archive-location "~/org/archive/%s::")
  (setq org-log-done t)

  (setq org-agenda-custom-commands
        '(("o" "At the office" tags-todo "@office"
           ((org-agenda-overriding-header "Office")))))

  (setq org-todo-keywords
        '("TODO(t)" "WAITING(z)" "|" "DONE(d)" "CANCELLED(c)"))

  (setq org-todo-keyword-faces
        '(("NEXT" :foreground "#79740E" :weight bold)
          ("WAITING" :foreground "#3C3246" :weight bold)))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sql . t)
     (shell . t)))

  :bind
  ("C-c a" . org-build-agenda)
  ("C-c c" . org-capture)
  (:map org-agenda-mode-map
        ("s-a"   . org-agenda-quit)
        ("f"     . org-agenda-filter-by-category))
  (:map org-mode-map
        ;; I accidentally hit this one quite a lot, and the `pcomplete'
        ;; bullshit sucks.
        ("C-M-i" . 'org-cycle)
        ("C-c C-x C-a" . 'org-archive-done-tasks)
        ("C-c d" . 'org-add-cookie))

  :hook
  (after-save-hook . th/org-update-agenda))

(use-package worf
  :init
  (add-hook 'org-mode-hook 'worf-mode))

(use-package org-journal
  ;; :bind (("C-." . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-find-file 'find-file))




(defun th/org-update-agenda ()
  (interactive)
  (ignore-errors
    (when (string-equal (f-ext (buffer-file-name)) "org")
      (org-agenda-redo-all t))))

(defun org-build-agenda ()
  (interactive)
  (org-agenda 0 "a"))



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
  (save-buffer)
  (message "Done tasks archived"))

(defun org-add-cookie (arg)
  "Adds a statistics cookie to the current heading"
  (interactive "P")
  (save-excursion
    (org-end-of-line)
    (if (looking-back "]")
        ;; Already exists, kill it
        (progn
          (backward-list)
          (backward-char)
          (org-kill-line))

      (insert (format " [%s]" (if arg "/" "%")))
      (org-ctrl-c-ctrl-c))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (and (!= n-done 0) (= n-not-done 0)) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defhydra th/org-hydra (:exit t)
  "Org commands"
  ("s-o" th/org-project "project file")
  ;; ("c" cfw:open-org-calendar "calendar")
  ("o" org-capture "capture")
  ("f" (projectile-switch-project-by-name org-directory) "files")
  ("s" (org-agenda nil "a") "schedule")
  ("i" (find-file "~/org/inbox.org") "inbox")
  ("j" org-journal-new-entry "journal")
  ("a" org-todo-list "agenda")
  ("t" org-tags-view "tags"))


(provide 'th-org-base)
