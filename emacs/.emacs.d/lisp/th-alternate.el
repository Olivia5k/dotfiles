(defun th/other-files-suffix (&optional suffix)
  "Browse between files of a certain kind in the current project.
Defaults to the suffix of the current buffer if none is given.

E.g. if you are visiting a .go file, this will list all other .go files.

This is useful if you have backend and frontend code in the same repo."

  (interactive)
  (find-file
   (concat
    (projectile-project-root)
    (let* ((suf (or suffix (f-ext (buffer-file-name))))
           (default-directory (projectile-project-root))
           (targets (-filter
                     (lambda (x)
                       (and
                        (s-suffix? (concat "." suf) x)
                        ;; Filter out test files, backup files and the current file
                        ;; (not (s-contains? "test" x))
                        (not (s-contains? ".#" x))
                        (not (s-contains? x (buffer-file-name)))))
                     (projectile-get-repo-files))))

      (cond
       ((= (length targets) 1)
        (car targets))
       ((= (length targets) 0)
        (error (format "No other %s files" suf)))
       (t
        (completing-read
         (format "%s files: " suf)
         targets)))))))

(global-set-key (kbd "C-x a") 'th/other-files-suffix)

(defun th/other-files-same-base ()
  "Find other files that have the same base as the current
one. Complete if there are multiple found.

E.g. if you are visiting `user.go' and `User.vue' exists, visit
that. If there is also a `UserPanel.vue', start completion
between the matching files instead.

This is useful if you have backend and frontend code in the same repo."

  (interactive)

  (let* ((base (f-base (buffer-file-name)))
         (default-directory (projectile-project-root))
         (files (-filter
                 (lambda (x)
                   (and
                    (s-prefix? (downcase base)
                               (downcase (f-base (f-filename x))))
                    ;; Filter out test files, backup files and the current file
                    (not (s-contains? "test" x))
                    (not (s-contains? ".#" x))
                    (not (s-contains? x (buffer-file-name)))))
                 (projectile-get-repo-files))))
    (cond
     ((= (length files) 1)
      (find-file (car files)))

     ((> (length files) 1)
      (find-file
       (completing-read "Alt files: " files)))

     (t
      (error "No alternate file for %s" (buffer-name))))))

(global-set-key (kbd "C-x C-a") 'th/other-files-same-base)

(provide 'th-alternate)
