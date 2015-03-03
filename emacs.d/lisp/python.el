(defun python-goto-or-add-docstring (&optional flags)
  "Go to the docstring for the current class or function. Create it if it does
not already exist."
 (interactive)
 (re-search-backward
   "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
  (next-line)
  (move-beginning-of-line)
  (if (looking-at "\"\"\"")
      (next-line)
    ((previous-line)
     (newline-and-indent)
     (insert "\"\"\"")
     (newline 2)
     (insert "\"\"\"")
     (previous-line 2))))

;(define-key python-mode (kbd "C-c d") 'python-goto-or-add-docstring)

(provide 'python)
