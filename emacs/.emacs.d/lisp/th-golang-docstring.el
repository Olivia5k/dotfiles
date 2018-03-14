(defun go-update-docstring ()
  "Update (or create) the docstring function name of the current function.

Designed to be called from hooks.

Will not update tests (beginning with Test/Example/Benchmark) or
private functions (lowercase)."
  (interactive)

  ;; Only run this hook when in go mode
  (when (and nil (eq major-mode 'go-mode))
    (save-excursion
      (let ((fn (go--function-name)))
        (when (go--should-generate-docstring-p fn)
          (go-goto-docstring)
          ;; Check if we need to update anything
          (when (and
                 (not (looking-at "$")) ;; If at the end of the line, the name has already been generated.
                 (not (looking-at (format "%s " fn)))) ;; If already looking at the correct name, then nothing changed.
            (kill-word 1)
            (insert fn)
            ;; If we updated and are at the end of the line, add a space.
            (if (looking-at "$")
                (insert " ")
              (forward-char 1))))))))

(defun go--should-generate-docstring-p (func-name)
  "Check if we should update the docstring or not"
  (and
   ;; If the function name is a test, skip it.
   (not (or (s-prefix? "Test" func-name)
            (s-prefix? "Example" func-name)
            (s-prefix? "Benchmark" func-name)))
   ;; If the function name is lowercase, then we don't need a docstring
   (not (s-lowercase? (s-left 1 func-name)))
   ;; We need to be at the definition line
   (and
    (progn
      (beginning-of-line)
      (looking-at "^func "))
    (progn
      (end-of-line)
      (backward-char 1)
      (looking-at "{$")))))

(defun go-delete-backward-char ()
  "runs `delete-backward-char' and also the docstring hook"
  (interactive)
  (delete-backward-char 1)
  (go-update-docstring))

(defun go-delete-char ()
  "runs `delete-char' and also the docstring hook"
  (interactive)
  (delete-char 1)
  (go-update-docstring))

(add-hook 'post-self-insert-hook 'go-update-docstring)


(provide 'th-golang-docstring)
