(defun python-goto-or-add-docstring (&optional flags)
  "Go to the docstring for the current class or function. Create it if it does
not already exist."
 (interactive)
 (re-search-backward
   "^\\([ ]*\\)\\(class\\|def\\) \\([a-zA-Z0-9_]+\\)" nil t)
  (next-line)
  (move-beginning-of-line 1)
  (if (looking-at "\"\"\"")
      (next-line)
    (progn

      (newline-and-indent)
      (previous-line))))
      ;; (insert "\"\"\"")
      ;; (newline-and-indent)
      ;; (newline-and-indent)
      ;; (insert "\"\"\"")
      ;; (newline-and-indent)
      ;; (newline-and-indent)
      ;; (previous-line 3))))

;(define-key python-mode (kbd "C-c d") 'python-goto-or-add-docstring)

(provide 'snakecharmer)
