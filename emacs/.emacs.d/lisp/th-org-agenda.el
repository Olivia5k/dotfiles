(use-package org-agenda
  :after org
  :straight nil
  :ensure nil
  :bind
  (("C-c a" . org-agenda)
   ("s-a" . org-build-agenda))
  (:map org-agenda-mode-map
        ("s-a"   . org-agenda-quit)
        ("f"     . org-agenda-filter-by-category))
  :init
  (setq org-agenda-ndays 7)
  (setq org-agenda-files '("~/org/"))
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-block-separator ?-)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-window-setup 'current-window)
  (setq org-archive-location "~/org/archive/%s::")
  (setq org-log-done t) )

;; http://cestlaz.github.io/posts/using-emacs-26-gcal/#.WIqBud9vGAk
(use-package org-gcal
  :after org-agenda
  :config
  (setq org-gcal-file-alist '(("lowe.thiderman@unomaly.com" . "~/org/gcal.org"))))

(use-package calfw)
(use-package calfw-org)

;; == bh/helper-functions ==
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

;; Some helper functions for selection within agenda views
(defun gs/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (outline-next-heading)
                               (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))

(defun gs/select-projects ()
  "Selects tasks which are project headers"
  (gs/select-with-tag-function #'bh/is-project-p))

(defun gs/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (gs/select-with-tag-function
   #'(lambda () (and
            (not (bh/is-project-p))
            (bh/is-project-subtree-p)))))

(defun gs/select-standalone-tasks ()
  "Skips tags which belong to projects. Is neither a project, nor does it blong to a project"
  (gs/select-with-tag-function
   #'(lambda () (and
            (not (bh/is-project-p))
            (not (bh/is-project-subtree-p))))))

(defun gs/select-projects-and-standalone-tasks ()
  "Skips tags which are not projects"
  (gs/select-with-tag-function
   #'(lambda () (or
            (bh/is-project-p)
            (bh/is-project-subtree-p)))))

(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
        (concat "{" loc "} ")
      "")))

(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
        (stuck (gs/org-agenda-project-warning))) ; warning for stuck projects
    (if (> (length path) 0)
        (concat stuck ; add stuck warning
                " [" path "]") ; add "breadcrumb"
      stuck)))

(defun gs/org-agenda-project-warning ()
  "Is a project stuck or waiting. If the project is not stuck,
show nothing. However, if it is stuck and waiting on something,
show this warning instead."
  (if (gs/org-agenda-project-is-stuck)
      (if (gs/org-agenda-project-is-waiting) " !W" " !S") ""))

(defun gs/org-agenda-project-is-stuck ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (has-next))
        (save-excursion
          (forward-line 1)
          (while (and (not has-next)
                      (< (point) subtree-end)
                      (re-search-forward "^\\*+ NEXT " subtree-end t))
            (unless (member "WAITING" (org-get-tags-at))
              (setq has-next t))))
        (if has-next nil t)) ; signify that this project is stuck
    nil))

(defun gs/org-agenda-project-is-waiting ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (save-excursion
          (re-search-forward "^\\*+ WAITING" subtree-end t)))
    nil))

(setq org-agenda-custom-commands
      '(("h" "Habits" agenda "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                                            (org-agenda-span 'day)
                                            (org-agenda-ndays 1)
                                            (org-agenda-start-on-weekday nil)
                                            (org-agenda-start-day "+0d")
                                            (org-agenda-todo-ignore-deadlines nil)))
                                (tags-todo "-INACTIVE-CANCELLED-SCHEDULED-ARCHIVE/!NEXT"
                                           ((org-agenda-overriding-header "Next Tasks:")
                                            ))
                                (tags "REFILE-ARCHIVE-REFILE=\"nil\""
                                      ((org-agenda-overriding-header "Tasks to Refile:")
                                       (org-tags-match-list-sublevels nil)))
                                (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
                                           ((org-agenda-overriding-header "Active Projects:")
                                            (org-agenda-skip-function 'gs/select-projects)))
                                (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
                                           ((org-agenda-overriding-header "Standalone Tasks:")
                                            (org-agenda-skip-function 'gs/select-standalone-tasks)))
                                (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
                                           ((org-agenda-overriding-header "Remaining Project Tasks:")
                                            (org-agenda-skip-function 'gs/select-project-tasks)))
                                (tags "INACTIVE-ARCHIVE"
                                      ((org-agenda-overriding-header "Inactive Projects and Tasks")
                                       (org-tags-match-list-sublevels nil)))
                                (tags "ENDOFAGENDA"
                                      ((org-agenda-overriding-header "End of Agenda")
                                       (org-tags-match-list-sublevels nil))))
         ((org-agenda-start-with-log-mode t)
          (org-agenda-log-mode-items '(clock))
          (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
                                      (timeline . "  % s")
                                      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
                                      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
                                      (search . "  %i %-12:c")))
          (org-agenda-todo-ignore-deadlines 'near)
          (org-agenda-todo-ignore-scheduled t)))
        ("X" "Agenda" ((agenda "") (alltodo))
         ((org-agenda-ndays 10)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "-1d")
          (org-agenda-start-with-log-mode t)
          (org-agenda-log-mode-items '(closed clock state)))
         )))

(defun org-build-agenda ()
  (interactive)
  (org-agenda 0 " "))


(provide 'th-org-agenda)
