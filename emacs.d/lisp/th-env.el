(defun th/load-env (&optional dir)
  "Loads all environment variables inside of given file into emacs."
  (interactive)

  (let* ((fn (th/find-env-file dir))
         (hash (th/get-env-hash fn))
         (keys (hash-table-keys hash)))
    (mapcar
     (lambda (k)
       (setenv k (gethash k hash)))
     keys)
    (message "Loaded %s.env: %s"
             (f-base fn)
             (s-join ", " keys))))

(defun th/find-env-file (&optional dir)
  "Find the .env file in the current directory.

     If `dir' is given, use that instead of current."

  (when (not dir)
    (setq dir default-directory))

  ;; Fetches the first file if there is only one - completing read if multiple.
  ;; TODO(thiderman): No error handling if there are no files
  (let* ((files (f-files dir
                         (lambda (file)
                           (equal (f-ext file) "env")))))
    (if (= 1 (length files))
        (car files)
      (completing-read "env: " files))))

(defun th/get-env-hash (fn)
  "For a given filename, return a list of acons of env variables inside."
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file fn)
      (beginning-of-buffer)
      (mapcar 'th/-put-into-hash
              (th/-get-env-lines-in-file)))
    table))

(defun th/-put-into-hash (s)
  (let* ((spl (s-split "=" (s-replace "export " "" s)))
         (key (car spl))
         (val (s-replace "\"" "" (cadr spl))))
    ;; `table' is set in the lexical scope of the callee
    (puthash key val table)))

(defun th/-get-env-lines-in-file ()
  (-filter
   (lambda (s) (s-starts-with? "export" s))
   (s-lines
    (buffer-substring-no-properties (point) (point-max)))))


(provide 'th-env)
