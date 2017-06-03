(setq alt-test-files
      '("path/one/file.el"
        "path/two/file.el"
        "path/one/file.py"
        "path/two/file.py"))

(setq alt-test-files (-map 'file-truename alt-test-files))
(defun alt-group-by-filename (input)
  (-group-by 'f-filename input))

(setq al (alt-group-by-filename alt-test-files))

(defun alt-keys (al)
  (-map 'car al))

(defun alt-for-filename (fn al)
  (let* ((alts (-alt-for-fn fn al)))
    (when (not alts)
      (error "No alt files for %s" fn))

    alts))

(defun -alt-for-fn (fn al)
  (remove (file-truename fn) (cdr (assoc (f-filename fn) al))))

(alt-for-filename "path/two/file.el" al)
