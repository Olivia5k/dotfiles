(require 'pytest)
(require 'string-inflection)
(require 's)
(require 'dash)

(defun python-goto-above-class-or-function ()
  "Do a backwards motion towards the above function or class definition"
  (re-search-backward
   "^\\( *\\)\\(class\\|def\\) \\([a-zA-Z0-9_]+\\)" nil t))

(defun snake-goto-or-add-docstring (&optional flags)
  "Go to the docstring for the current class or function. Create it if it does
   not already exist."
  (interactive)
  (python-goto-above-class-or-function)
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
  (newline-and-indent)
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

(defun snake-goto-test ()
  "Based on the current function and class, goto to the test class in the
   corresponding test file.

   If the test class does not exist, it will be created. If the implementation
   class has previous tests, preserve the ancestor class of those previous tests."

  (interactive)
  (progn
    (find-file (snake-alternate-file))))

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

(defun snake-sync-arguments ()
  "Modify or arrange the arguments for the current function.

  If the function is a test, check if there are @mock.patch() decorators and
  change the arguments to the function to reflect the injected mocks.
  If the function is an __init__, make sure that all arguments are assigned
  as `self.arg = arg`")

(defvar snakecharmer-map (make-sparse-keymap)
  "snakecharmer keymap")
(define-key snakecharmer-map
  (kbd "C-c d") 'snake-goto-or-add-docstring)
(define-key snakecharmer-map
  (kbd "C-c n") 'snake-toggle-nocover)
(define-key snakecharmer-map
  (kbd "M-RET") 'snake-goto-test)

(define-minor-mode snakecharmer-mode
  "Snakecharmer mode mode" nil " charm" snakecharmer-map)

(add-hook 'python-mode-hook 'snakecharmer-mode)

(provide 'snakecharmer)
