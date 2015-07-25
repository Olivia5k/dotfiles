(require 'pytest)
(require 'filenotify)
(require 'string-inflection)
(require 's)
(require 'dash)
(require 'async)


(defun snake-goto-above-class-or-function ()
  "Do a backwards motion towards the above function or class definition"
  (re-search-backward
   "^\\( *\\)\\(class\\|def\\) \\([a-zA-Z0-9_]+\\)" nil t))

(defun snake-goto-or-add-docstring (&optional flags)
  "Go to the docstring for the current class or function. Create it if it does
   not already exist."
  (interactive)
  (snake-goto-above-class-or-function)
  (forward-to-indentation 1)
  (if (looking-at "\"\"\"")
      (next-line)
    (progn
      (previous-line)
      (snake-create-docstring))))

(defun snake-create-docstring ()
  "Insert an indented docstring under the current line."
  (end-of-line)
  (newline-and-indent)
  (insert "\"\"\"")
  (newline-and-indent)
  (insert "")
  (newline-and-indent)
  (newline-and-indent)
  (insert "\"\"\"")
  (newline)
  (previous-line 3)
  (indent-for-tab-command))

(defun snake-toggle-nocover ()
  "Toggle a '# pragma: nocover' comment on the current block"
  (interactive)
  (let ((token "  # pragma: nocover"))
    (save-excursion
      (end-of-line)
      (if (not (looking-at-p ":$"))
          (progn
            (re-search-backward (format ":\\(%s\\)\?$" token) nil t)
            (end-of-line)))
      (backward-word 2)
      (backward-char 4)
      (if (looking-at token)
          (kill-line)
        (progn
          (end-of-line)
          (insert token))))))

(defun snake-toggle-self ()
  "Toggle 'self.' on the current symbol. If on a line with def, toggle as first argument."
  (interactive)
  (save-excursion
    (let ((symbol (symbol-at-point)))
      (if (looking-back "^\s\+def .*")
          (snake-toggle-def-self)
        (if symbol
            (if (s-equals? symbol "self")
                (snake-remove-self)
              (forward-char)
              (backward-word)
              (if (looking-back "self\.")
                  (progn
                    (backward-word)
                    (snake-remove-self))
                (insert "self."))))))))

(defun snake-remove-self ()
  "Remove 'self. from current symbol"
  (if (looking-back "\\w")
      (backward-word))
  (kill-word 1)
  (delete-char 1))

(defun snake-toggle-def-self ()
  "Toggle 'self' as the first argument to the function defined on the current line"
  (beginning-of-line)
  (re-search-forward "(" nil t)
  (if (looking-at "self")
      (progn
        (delete-char 4)
        (if (looking-at ", ")
            (delete-char 2)))
    (if (looking-at ")")
        (insert "self")
      (insert "self, "))))

(defun snake-goto-test ()
  "Based on the current function and class, goto to the test class in the
   corresponding test file.

   If the test class does not exist, it will be created. If the implementation
   class has previous tests, preserve the ancestor class of those previous tests."

  (interactive)
  (let* ((classes (snake-get-current-test-items))
         (other-file (snake-alternate-file))
         (line))
    (delete-other-windows)
    (split-window-right)
    (windmove-right)
    (find-file other-file)
    (if (s-contains? "test_" other-file)
        (snake-goto-or-create-test classes)
      (snake-run-single-test))
    (recenter)))

(defun snake-run-single-test ()
  "Execute the test the point is currently at."

  (interactive)
  (let ((class (cdr (outer-testable)))
         (func (cdr (inner-testable)))
         (fn (buffer-file-name)))
    (snake-exec-test (s-join "::" (list fn class func)))))

(defun snake-exec-test (&rest args)
  (cd (snake-find-root))
  (let* ((base "bin/py.test --result-log=pytest-results.log")
         (extra (s-join " " args))
         (cmd (s-join " " (list base extra))))
    (async-start
     (shell-command (format "%s &> /dev/null &" cmd) nil nil))))

(defun snake-get-current-test-items ()
  "Get the current class and function definition as if they were items of a
   test definition"
  (mapcar 'string-inflection-camelcase-function
          (list (cdr (outer-testable))
                (cdr (inner-testable)))))

(defun snake-alternate-file ()
  "If in application code, get the name of the corresponding test file and vice
   versa."
  (let* ((root (pytest-find-project-root))
         (file (s-chop-prefix root (buffer-file-name)))
         (parts (s-split "/" file)))
    (file-truename
     (s-join "/"
             (if (s-prefix? "test/" file)
                 (append                ; In test file - get path to code
                  (list root)
                  (last (s-split "/" root) 2)
                  (butlast (cdr parts))
                  (list (s-chop-prefix "test_" (car (last parts)))))
               (append                  ; In the code - get path to test file
                (list root "test")
                (butlast (cdr parts))
                (list (concat "test_" (car (last parts))))))))))

(defun snake-goto-or-create-test (classes)
  "Go to or create a test based on input classes.

   If point is found inside the test class from the implementation class we
   just switched from, the point will not be moved.

   If no test is found `snake-create-test` will be run."

  (let* ((class (s-join "" classes))
         (prefix (concat "class Test" class))
         (current-class (cdr (outer-testable)))
         (found nil))
    (if (not (s-equals? (format "Test%s" class) current-class))
        (if (not (snake-line-starting-with prefix))
            (snake-create-test class)))))

(defun snake-create-test (class)
  "Create a new test at the bottom of the current file."
  (end-of-buffer)
  (insert (format "\n\nclass Test%s:\n    " class))
  (end-of-buffer)
  (end-of-line))

(defun snake-line-starting-with (prefix)
  "Find a line starting with `prefix` in the current buffer. If found, set
   point to beginning of that line.

   Returns `t` if found, `nil` if not."
  (let ((found nil))
    (beginning-of-buffer)
    (while (and (not found) (not (= (point) (point-max))))
      (if (s-prefix?
           prefix
           (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          ;; Line found and scrolling will stop on the target line
          (setq found t)
        (forward-line 1)))
    found))

(defun snake-sync-arguments ()
  "Modify or arrange the arguments for the current function.

  If the function is a test, check if there are @mock.patch() decorators and
  change the arguments to the function to reflect the injected mocks.
  If the function is an __init__, make sure that all arguments are assigned
  as `self.arg = arg`")

(defun snake-goto-or-add-setup-method ()
  "Add a `def setup_method()` for the current test class")

;;; File watching
(defun snake-logfile-callback (filename)
  "Parse the logfile and decorate buffers accordingly.

This will read the results from the pytest-results.log file and add markers
denoting success or failure to the tests that have been run."
  (find-file-read-only filename)
  (beginning-of-buffer)
  (cl-loop until (eobp) do
           (snake-parse-result-line
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position)))
           (forward-line 1)))

(defun snake-decorate-test (file class func status)
  ""

  (save-excursion
    (find-file (format "/home/thiderman/git/piper/%s" file))
    (beginning-of-buffer)
    (re-search-forward (format "^class %s(" class) nil t)
    (re-search-forward (format "^    def \\(%s\\)(" func) nil t)

    (put-text-property
     (match-beginning 1) (match-end 1)
     'font-lock-face `((:foreground ,(if (s-equals? status ".")
                                         "#a7ff85" "#ff8585"))))))

(defun snake-parse-result-line (line)
  ""

  (if (not (s-starts-with? " " line))
      (let* ((spl (s-split " " line))
             (status (car spl))
             (data (s-split "::" (cadr spl)))
             (file (nth 0 data))
             (class (nth 1 data))
             (func (nth 3 data)))
        (if (s-equals? status ".")
            (snake-decorate-test file class func status)
          (snake-decorate-test file class func status)))))

(defun snake-watch-logfile ()
  "Watch a pytest results file for a project and register the updater callback
to it."
  (file-notify-add-watch
   ; TODO: Not hardcode, lel
   "/home/thiderman/git/piper/pytest-results.log"
   '(change attribute-change) (lambda (event)
                                (snake-logfile-callback (nth 3 event)))))

;;; Utility functions
(defun snake-find-root (&optional dirname)
  "Find the root of the project, based on where setup.py is. "
  (let ((dn
         (if dirname
             dirname
           (file-name-directory buffer-file-name))))
    (cond ((file-exists-p (concat dn "/setup.py")) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
          (t (snake-find-root
              (file-name-directory (directory-file-name dn)))))))

(defvar snakecharmer-map (make-sparse-keymap)
  "snakecharmer keymap")
(define-key snakecharmer-map
  (kbd "C-c d") 'snake-goto-or-add-docstring)
(define-key snakecharmer-map
  (kbd "C-c n") 'snake-toggle-nocover)
(define-key snakecharmer-map
  (kbd "M-RET") 'snake-goto-test)
(define-key snakecharmer-map
  (kbd "C-c C-c") 'snake-run-single-test)
(define-key snakecharmer-map
  (kbd "M-s") 'snake-toggle-self)

(global-set-key (kbd "C-c <C-return>") (lambda ()
                       (interactive)
                       (snake-logfile-callback "/home/thiderman/git/piper/pytest-results.log")))

(define-minor-mode snakecharmer-mode
  "Snakecharmer mode" nil " snake" snakecharmer-map
  (cd (snake-find-root)))

(add-hook 'python-mode-hook 'snakecharmer-mode)

(provide 'snakecharmer)
