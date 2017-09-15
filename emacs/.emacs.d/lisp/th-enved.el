(defvar enved-protected-variables
  '("DBUS_SESSION_BUS_ADDRESS"
    "GOPATH"
    "HOME"
    "LANG"
    "LOGNAME"
    "PATH"
    "SHELL"
    "SSH_AGENT_PID"
    "SSH_AUTH_SOCK"
    "TERM"
    "USER"
    "XDG_RUNTIME_DIR")
  "Environment variables one usually will not modify.")

(defvar enved-buffer-name "*enved*")

;;;###autoload
(defun enved ()
  "Opens a special buffer in which you can edit environment variables"
  (interactive)

  (switch-to-buffer enved-buffer-name)
  (enved-insert)
  (enved-mode))

;;;###autoload
(defun enved-load (&optional dir)
  "Loads all environment variables of the file into the emacs environment."
  (interactive)

  (let* ((fn (enved-find-file dir))
         (hash (enved-get-hash fn))
         (keys (hash-table-keys hash))
         (buf (get-buffer enved-buffer-name)))
    (mapcar (lambda (k) (setenv k (gethash k hash))) keys)

    ;; If enved is already loaded, update it
    (when buf
      (with-current-buffer buf
        (enved-insert)))

    (message "Loaded %s.env: %s" (f-base fn) (s-join ", " keys))))

(defun enved-insert ()
  "Load the current environment into the buffer"
  (kill-region (point-min) (point-max))

  (let* ((env (enved-split-environment))
         (edit (cdr (car env)))
         (noedit (cdr (cadr env))))

    (insert "# Current emacs process environment\n")
    (insert
     (s-join "\n" (-sort (lambda (x y) (string< x y)) edit)))

    (insert "\n\n# These are variables you can but probably wouldn't edit\n")
    (insert
     (s-join "\n" (-sort (lambda (x y) (string< x y)) noedit)))))

(defun enved-split-environment ()
  "Splits the current `process-environment' into two parts: editable
  and usually-not-editable."

  (-group-by
   (lambda (l)
     ;; Admittedly not very elegant, but it does the job. I couldn't
     ;; find a `member'-like function that just returned boolean.
     (not (eq nil (member (car (s-split "=" l))
                          enved-protected-variables))))
   process-environment))

;;;###autoload
(defun enved-set ()
  "Applies the changes to the environment."
  (interactive)
  (mapcar 'enved-process (split-string (buffer-string) "\n"))
  ;; Do a rewrite of the buffer
  (enved-insert))

(defun enved-process (line)
  "Processes a single line to see if it should be set or not."

  (let* ((spl (split-string line "="))
         (env (car spl))
         (val (cadr spl)))
    (when (not (string= val (getenv env)))
      (setenv env val t)
      (message "Updated %s to %s" env val))))

(defun enved-find-file (&optional dir)
  "Find .env files in the current directory.

If `dir' is given, use that instead of current."

  (when (not dir)
    (setq dir default-directory))

  ;; Fetches the first file if there is only one - completing read if multiple.
  (let* ((files (f-files dir (lambda (file) (equal (f-ext file) "env")))))
    (cond
     ((= 0 (length files))
      (error "No env files found."))
     ((= 1 (length files))
      (car files))
     (t
      (completing-read "env: " files)))))

(defun enved-get-hash (fn)
  "For a given filename, return a hash of env variables inside."
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file fn)
      (beginning-of-buffer)
      (mapcar 'enved--put-hash
              (enved--get-env-lines-in-file)))
    table))

(defun enved--put-hash (s)
  (let* ((spl (s-split "=" (s-replace "export " "" s)))
         (key (car spl))
         (val (s-replace "\"" "" (cadr spl))))
    ;; `table' is set in the lexical scope of the callee
    (puthash key val table)))

(defun enved--get-env-lines-in-file ()
  (-filter
   (lambda (s) (s-starts-with? "export" s))
   (s-lines
    (buffer-substring-no-properties (point) (point-max)))))

(define-derived-mode enved-mode prog-mode "enved"
  "Major mode for editing environment variables."
  (font-lock-add-keywords nil enved-keywords)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(defvar enved-keywords
  '(("^#.*$" . font-lock-comment-face)
    ("^[^= \n]+" . font-lock-function-name-face)
    ("[^= \n]+$" . font-lock-string-face)
    ("=" . font-lock-comment-face)))

(defvar enved-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-r") #'enved)
    (define-key m (kbd "C-c C-c") #'enved-set)
    m)
  "Keymap used by ‘enved-mode’.")

(provide 'th-enved)
