(defun go-refactor-wrap-if ()
  "Wraps the current line in an if statement."
  (interactive)
  (go--refactor-wrap "if ")
  (forward-char 3))

(defun go-refactor-wrap-for ()
  "Wraps the current line in a for loop."
  (interactive)
  (go--refactor-wrap "for ")
  (forward-char 4))

(defun go-refactor-wrap-goroutine ()
  "Wraps the current line in a goroutine."
  (interactive)
  (go--refactor-wrap "go func()")

  ;; Also add the parenthesis at the end
  (save-excursion
    (end-of-line)
    (backward-char 1)
    (forward-list)
    (insert "()"))

  (forward-char 8))

(defun go-refactor-wrap-errif ()
  "Wraps the current statement in an ~err := <x> ; err != nil {~ block."
  ;; TODO(thiderman): Would be really neat if this could use inspection to
  ;; figure out if the statement actually returns an error or not
  (interactive)
  (beginning-of-line-text)
  (insert "err := ")
  (end-of-line)
  (insert "; err != nil {}")
  (backward-char 1)
  (newline-and-indent))

(defun go-refactor-unwrap ()
  "Take the current statement (or region) and raise it to replace its parent.

Naively tries to figure out what the opening statement is by finding
the previous line ending with an opening brace."

  (interactive)
  (let* ((pos (go--region-or-lines))
         (beg (car pos))
         (end (cadr pos)))

    ;; Figure out if we can actually do a raise
    (save-excursion
      (goto-char beg)
      ;; If looking back at the beginning of the line or just a single tab, we
      ;; cannot raise because that would always produce an unusable end result.
      (when (looking-back "^\t?")
        (error "Cannot raise top-level statements")))

    (kill-region beg end)
    (re-search-backward "{$" nil t)

    ;; Find out where the wrapping statement is and delete it
    (save-excursion
      (beginning-of-line-text)
      (setq beg (point)))
    (forward-list 1)
    (setq end (point))
    (delete-region beg end)

    ;; Finally, yank out the original statements.
    ;; Out of laziness, also run gofmt to make them prettier again.
    (yank)
    (gofmt)))

(defun go-refactor-extract-variable ()
  "Extract the current region into a variable"
  (interactive)

  (when (not (region-active-p))
    (error "No region active"))

  (save-excursion
    (let* ((var (read-string "Variable name: ")))
      (kill-region (region-beginning) (region-end))
      (insert var)

      (beginning-of-line-text)
      (newline-and-indent)
      (previous-line)
      (indent-according-to-mode)

      (insert (format "%s := " var))
      (yank))))

(defun go-refactor-declaration-colon ()
  "Toggle if the current line should use \"=\" or \":=\"."
  (interactive)
  (save-excursion
    (let (beg end)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))

      (when (not (s-contains? "=" (buffer-substring beg end)))
        (error "No declaration on current line"))

      (search-forward "=")
      (backward-char 1)
      (if (looking-back ":" 1)
          (delete-char -1)
        (insert ":")))))

(defun go-refactor-toggle-nolint ()
  "Toggle the '// nolint' token on a line."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back "// nolint")
        (progn
          (backward-char 10)
          (kill-line))
      (insert " // nolint"))))

(defun go-refactor-method-receiver ()
  "Changes or removes the method receiver of the current function.

A choice between all the types in the current file are
interactively presented. Also presented is an item `<none>',
which will remove the receiver if there is one.

If there was a receiver and a new one is chosen, all variables are changed."
  ;; TODO(thiderman): We need to undo twice to undo this. Investigate.
  (interactive)
  (save-excursion
    (go-goto-function t)
    (forward-char 5)

    (let*
        ((empty "<none>")
         (current-var
          (save-excursion
            (forward-char 1)
            (thing-at-point 'symbol t)))
         (current-type
          (save-excursion
            (forward-char 1)
            (forward-word 2)
            (thing-at-point 'symbol t)))
         (type (completing-read
                "Type: "
                (append (go--get-types (buffer-file-name) current-type)
                        (list empty))))
         (receiver (when (not (s-equals? type empty))
                     (go--convert-type-name-to-receiver
                      (car (s-split " " type))))))

      (cond
       ;; If we are looking at an opening parenthesis, there is already a method receiver
       ((looking-at "(")
        ;; Firstly, store the current receiver variable name.


        ;; Then, delete the existing one.
        (delete-region
         (point)
         (save-excursion
           (forward-list 1)
           (point)))
        ;; If we do not have a receiver (i.e. we chose 'empty) we should
        ;; delete the extra space.
        (if (not receiver)
            (delete-char 1)
          ;; If there was a receiver previously and we set a new one, update the
          ;; variable name.
          (insert receiver)

          ;; And also update the variable name inside of the function.
          (when (and current-var receiver)
            (go--refactor-symbol-in-function
             current-var
             (s-downcase (s-left 1 type))))))
       ((and (not (looking-at "(")) receiver)
        ;; There is no receiver, but we are adding one. Just insert it.
        (insert (format "%s " receiver)))))))

(defun go-refactor-error-withstack ()
  "Turns 'err' to 'errors.WithStack(err)'."
  (interactive)
  (save-excursion
    (unless (looking-at "err")
      (backward-word))
    (insert "errors.WithStack(")
    (forward-word)
    (insert ")")))

(defun go--refactor-symbol-in-function (from to)
  "Changes instances of the symbol `from' into `to'.

Assumes that point is on line defining the function we are replacing in."
  (save-excursion
    (beginning-of-line)
    (let ((start
           (save-excursion
             (forward-line -1)
             (point)))
          (end
           (save-excursion
             ;; TODO(thiderman): Make a method that reliably moves to opening brace.
             (end-of-line)
             ;; In case of trailing whitespace...
             (search-backward "{")
             (forward-list 1)
             (backward-char 1)
             (point))))

      (replace-string from to t start end))))

(defun go--refactor-wrap (prefix)
  "Wraps the current line or region or statement in a templated statement.

If the current line ends in an opening brace, the entire
statement until that brace's end will be wrapped.

Point ends up on the beginning of the templated statement."
  (interactive)
  (save-excursion
    (let* ((pos (go--region-or-lines))
           (beg (car pos))
           (end (cadr pos)))
      (kill-region beg end)
      (indent-according-to-mode)
      (insert (format "%s {}" prefix))
      (backward-char 1)
      (newline-and-indent)
      (yank)))

  (beginning-of-line-text)

  ;; Indent the things we just wrapped
  (indent-region
   (point)
   (save-excursion
     (end-of-line)
     (backward-char 1)
     (forward-list)
     (point))))

(defun go--region-or-lines ()
  "Operate or regions or lines"

  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (save-excursion
                (back-to-indentation)
                (point)))
    (if mark-active
        (exchange-point-and-mark))
    ;; If we're on a line that ends on an opening brace, set the end to
    ;; be the outside of that brace.
    (setq end
          (save-excursion
            (if (progn (end-of-line)
                       (backward-char 1)
                       (looking-at "{"))
                (progn
                  (forward-list)
                  (point))
              (line-end-position))))
    (list beg end)))

(defun go--convert-type-name-to-receiver (tn)
  "Converts from the string \"Type\" to \"(t *Type)\""
  (format "(%s *%s)" (s-downcase (s-left 1 tn)) tn))

(defun go--get-types (&optional file-name skip-type)
  "Return a list of all the types found in the current file.

The strings returned are based on all lines that begin with
'^type'. The letters 'type ' and the ending ' {' are both
removed.

If `skip-type' is provided, that type will not be included in the
resulting list."

  (save-excursion
    (let ((fn (or file-name (buffer-file-name))))
      (-map
       (lambda (s) (s-chop-suffix " {" (s-chop-prefix "type " s)))
       (-filter
        (lambda (s)
          (if skip-type
              ;; If skip-type is provided, also filter out that line
              (and (s-prefix? "type " s)
                   (not (s-prefix? (format "type %s " skip-type) s)))
            ;; Otherwise just return lines that start with "type"
            (s-prefix? "type " s)))

        ;; Does emacs really don't have a cleaner way of getting lines in a
        ;; file? :/
        (with-temp-buffer
          (insert-file-contents fn)
          (split-string (buffer-string) "\n" t)))))))

(provide 'th-golang-refactor)
