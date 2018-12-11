(use-package org
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

  (setq org-refile-targets
      '((org-agenda-files :maxlevel . 5)))

  (setq org-agenda-ndays 7)
  (setq org-agenda-files '("~/org/"))
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-block-separator ?-)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-window-setup 'current-window)
  (setq org-archive-location "~/org/archive/%s::")
  (setq org-log-done t)

  (setq org-agenda-custom-commands nil)

  (setq org-todo-keywords
        '("TODO(t)" "NEXT(n)" "WAITING(z)" "REVIEW(r)" "|" "DONE(d)" "INVALID(i)"))

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
  (:map org-agenda-mode-map
        ("s-a"   . org-agenda-quit)
        ("f"     . org-agenda-filter-by-category))
  (:map org-mode-map
        ;; I accidentally hit this one quite a lot, and the `pcomplete'
        ;; bullshit sucks.
        ("C-M-i" . 'org-cycle)
        ("C-c C-x C-a" . 'org-archive-done-tasks))

  :hook
  ('after-save-hook . #'th/org-update-agenda))

(use-package worf
  :init
  (add-hook 'org-mode-hook 'worf-mode))

(use-package org-clock
  :straight nil
  :ensure nil)

(use-package org-journal
  ;; :bind (("C-." . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-find-file 'find-file))

;; (use-package org-habit
;;   :ensure nil
;;   :straight nil
;;   :init
;;   (setq org-modules '(org-habit))
;;   (setq org-habit-show-habits-only-for-today t))

;; (use-package org-bullets)

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODO")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "bills"
                :priority "A")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "Shopping in town"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :tag "@town"))
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          (:name "Space-related (non-moon-or-planet-related)"
                                 ;; Regexps match case-insensitively on the entire entry
                                 :and (:regexp ("space" "NASA")
                                               ;; Boolean NOT also has implicit OR between selectors
                                               :not (:regexp "moon" :tag "planet")))))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         )))

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

   ("w" "Track weight" entry
    (file+headline "~/org/irl.org" "Weight")
    "* %T %^{Current weight} kg"
    :immediate-finish t)

   ("r" "Recipe" entry
    (file+headline "~/org/food.org" "Recipes")
    "* [[%^{URL}][%^{Title}]]"
    )

   ("l" "Line" entry
    (file "~/org/code.org")
    "* [[file://%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%^{Description}]]"
    )))


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
