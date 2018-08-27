(defun th/other-files (&optional pattern)
  "Browse between files matching a pattern in the current project.

Defaults to the file suffix of the current buffer if none is given.

E.g. if you are visiting a .go file, this will list all other .go files."

  (interactive)
  (find-file
   (concat
    (projectile-project-root)
    (let* ((bn (buffer-file-name))
           ;; The pattern, the suffix, or the filename
           ;; This is to make Makefiles and Dockerfiles work,
           (pat (or pattern (f-ext bn) (f-base bn)))
           (default-directory (projectile-project-root))
           (targets (-filter
                     (lambda (x)
                       (s-contains? pat x))
                     (projectile-get-repo-files))))

      (cond
       ((= (length targets) 1)
        (car targets))
       ((= (length targets) 0)
        (error (format "No other files matching '%s'" pat)))
       (t
        (completing-read
         (format "%s files: " pat)
         targets)))))))

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


(defun th/browse-suffixes ()
  "Select a suffix from the project and then browse files with that suffix."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (suffixes (remove nil
                           (-distinct
                            (-map (lambda (x) (f-ext x))
                                  (projectile-get-repo-files))))))
    (th/other-files
     (completing-read "suffixes: " suffixes nil t))))

(provide 'th-alternate)
