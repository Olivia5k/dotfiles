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
  "Toggle a '# pragma: nocover' comment at the end of the current line"
  (interactive)
  (let ((token "  # pragma: nocover"))
    (save-excursion
      (end-of-line)
      ; Inelegance galore. <3
      (backward-word 2)
      (backward-char 4)
      (if (looking-at token)
          (kill-line)
        (progn
          (end-of-line)
          (insert token))))))

(defun snake-get-position ()
  "Return a list with the the parent functions and/or classes above point"
  (interactive)
  (save-excursion
    (python-goto-above-class-or-function)
    (message "%s hehe" (match-data 2))))

(defvar snakecharmer-map (make-sparse-keymap)
  "snakecharmer keymap")

(define-key snakecharmer-map
  (kbd "C-c d") 'snake-goto-or-add-docstring)
(define-key snakecharmer-map
  (kbd "C-c n") 'snake-toggle-nocover)

(define-minor-mode snakecharmer-mode
  "Snakecharmer mode mode" nil " charm" snakecharmer-map)

(provide 'snakecharmer)
