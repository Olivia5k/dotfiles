(defun snake-goto-or-add-docstring (&optional flags)
  "Go to the docstring for the current class or function. Create it if it does
   not already exist."
  (interactive)
  (re-search-backward
   "^\\( *\\)\\(class\\|def\\) \\([a-zA-Z0-9_]+\\)" nil t)
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

;(define-key python-mode (kbd "C-c d") 'python-goto-or-add-docstring)

(provide 'snakecharmer)
