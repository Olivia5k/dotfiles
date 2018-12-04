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
           (files (-filter
                     (lambda (x)
                       (s-contains? pat x))
                     (projectile-dir-files-alien (projectile-project-root)))))

      (cond
       ((= (length files) 1)
        (car files))
       ((= (length files) 0)
        (error (format "No files matching '%s'" pat)))
       (t
        (completing-read
         (format "%s files: " pat)
         files)))))))

(defun th/other-files-same-base ()
  "Find other files that have the same base as the current
one. Complete if there are multiple found.

E.g. if you are visiting `user.go' and `User.vue' exists, visit
that. If there is also a `UserPanel.vue', start completion
between the matching files instead.

This is useful if you have backend and frontend code in the same repo."

  (interactive)

  (let* ((base (f-base (buffer-file-name)))
         (files (-filter
                 (lambda (x)
                   (and
                    (s-prefix? (downcase base)
                               (downcase (f-base (f-filename x))))
                    ;; Filter out test files, backup files and the current file
                    (not (s-contains? "test" x))
                    (not (s-contains? ".#" x))
                    (not (s-contains? x (buffer-file-name)))))
                 (projectile-dir-files-alien (projectile-project-root)))))
    (cond ((= (length files) 1)
           (find-file (car files)))

          ((> (length files) 1)
           (find-file
            (completing-read "Alt files: " files)))

          (t
           (error "No alternate files for %s" (buffer-name))))))


(defun th/browse-extensions ()
  "Select a file extension from the project and browse files with that extension."
  (interactive)
  (let* ((suffixes
          (remove nil
                  (-distinct
                   (-map (lambda (x) (format ".%s" (f-ext x)))
                         (projectile-dir-files-alien (projectile-project-root)))))))
    (th/other-files
     (completing-read "suffixes: " suffixes nil t))))

(provide 'th-alternate)
