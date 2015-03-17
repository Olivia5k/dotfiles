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
      (insert "\"\"\"\n%s\n\n\"\"\"\n\n"))))

;(define-key python-mode (kbd "C-c d") 'python-goto-or-add-docstring)

(provide 'snakecharmer)
