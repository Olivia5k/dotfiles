(use-package go-mode
  :bind
  (:map go-mode-map
        ("C-c C-i" . go-remove-unused-imports)
        ("C-c C-j" . godef-jump-other-window)
        ("C-c C-o" . th/go-guru/body)
        ("C-c c"   . th/go-coverage)
        ("C-c i"   . go-goto-imports)
        ("C-c l"   . go-refactor-toggle-nolint)
        ("C-c r"   . go-rename)
        ("M-."     . godef-jump)
        ("M-m"     . go-goto-hydra/body)

        ("C-M-x"   . th/go-single-test)
        ("C-c C-c" . makefile-executor-execute-last)
        ("C-c C-b" . th/go-benchmark)
        ("C-c C-l" . th/go-lint)
        ("C-c C-r" . th/go-race)

        ("DEL"     . go-delete-backward-char)
        ("C-d"     . go-delete-char)

        ("C-c C-m" . go-refactor-map)
        ("C-M-d"   . go-refactor-declaration-colon)
        ("M-r"     . go-refactor-unwrap)
        ("C-c s"   . th/go-server-hydra/body)
        ("C-c a"   . ff-find-other-file))

  :config
  (add-hook 'go-mode-hook 'th/go-hook)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'flycheck-mode)

  (setq gofmt-command "goimports")
  (setq gofmt-args "")

  (define-prefix-command 'go-refactor-map)
  (define-key go-refactor-map (kbd "i") 'go-refactor-wrap-if)
  (define-key go-refactor-map (kbd "f") 'go-refactor-wrap-for)
  (define-key go-refactor-map (kbd "g") 'go-refactor-wrap-goroutine)
  (define-key go-refactor-map (kbd "e") 'go-refactor-wrap-errif)
  (define-key go-refactor-map (kbd "u") 'go-refactor-unwrap)
  (define-key go-refactor-map (kbd "v") 'go-refactor-extract-variable)
  (define-key go-refactor-map (kbd "d") 'go-refactor-declaration-colon)
  (define-key go-refactor-map (kbd "r") 'go-refactor-method-receiver)
  (define-key go-refactor-map (kbd "w") 'go-refactor-error-withstack))

(use-package go-rename
  :after go-mode
  :config
  (defadvice go-rename (before save-go-rename activate)
    "Save the buffer before calling go-rename."
    (save-buffer)))

(use-package company-go :after go-mode)
(use-package go-guru :after go-mode)
(use-package go-eldoc :after go-mode)
(use-package go-impl
  :after go-mode
  :bind (:map go-mode-map ("C-c M-i" . go-impl)))
(use-package go-add-tags
  :after go-mode
  :bind (:map go-mode-map ("C-c t" . go-add-tags)))

(use-package flycheck-gometalinter
  :ensure t
  :init
  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq flycheck-gometalinter-vendor t)
  ;; only show errors
  (setq flycheck-gometalinter-errors-only t)
  ;; only run fast linters
  (setq flycheck-gometalinter-fast t)
  ;; use in tests files
  (setq flycheck-gometalinter-test t)
  (setq flycheck-gometalinter-deadline "5s")
  :config
  (progn
    (flycheck-gometalinter-setup)))

(defun th/go-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))

(defhydra go-goto-hydra (:foreign-keys warn)
  "goto"
  ("a" go-goto-arguments "arguments")
  ("d" go-goto-docstring "docstring")
  ("f" go-goto-function "function")
  ("i" go-goto-imports "imports")
  ("m" go-goto-method-receiver "receiver")
  ("n" go-goto-function-name "name")
  ("r" go-goto-return-values "return")
  ;; ("b" pop-mark "back")
  ("q" nil))

(defhydra th/go-guru (:exit t :columns 3)
  "Guru commands"
  ("d" go-guru-describe "describe")
  ("f" go-guru-freevars "freevars")
  ("i" go-guru-implements "implements")
  ("c" go-guru-peers "peers (channels)")
  ("r" go-guru-referrers "referrers")
  ("j" go-guru-definition "definition")
  ("p" go-guru-pointsto "pointsto")
  ("s" go-guru-callstack "callstack")
  ("e" go-guru-whicherrs "whicherrs")
  ("<" go-guru-callers "callers")
  (">" go-guru-callees "callees")
  ("x" go-guru-expand-region "expand-region"))

(defhydra th/go-server-hydra (:exit t)
  ("c" th/go-server-compile "compile")
  ("s" th/go-server-start "start")
  ("k" th/go-server-stop "stop")
  ("b" th/go-server-buffer "buffer")
  ("n" th/npm-server-start "npm start"))

(defun th/go-coverage ()
  "Toggle coverage mode for the current buffer"
  (interactive)

  (save-excursion
    (let ((bn (buffer-name)))
      (if (s-contains? "<gocov>" bn)
          (progn
            (windmove-left)
            (th/go-test-buffer-p))

        (if (s-contains? "_test.go" bn)
            (th/go-coverage-toggle)
          (windmove-right)
          (if (not (s-contains? "<gocov>" bn))
              (th/go-coverage-toggle)
            (windmove-left)
            (th/go-test-buffer-p)))))))

(defun th/go-coverage-toggle ()
  (delete-other-windows)
  (go-coverage "coverage.out"))

(defun th/go-test-buffer ()
  (interactive)
  (let* ((bname (buffer-file-name))
         (left "")
         (right ""))

    (if (s-suffix? "_test.go" bname)
        (setq left (th/go-alternate-file bname)
              right bname)
      (setq left bname
            right (th/go-alternate-file bname)))

    (find-file left)
    (delete-other-windows)
    (split-window-horizontally)
    (windmove-right)
    (find-file right)))

(defun th/go-test-buffer-split (file)
  ;; If we happen to be on the test file when splitting, go left once
  (when (th/go-test-file-p (buffer-file-name (get-buffer)))
    (windmove-left))

  (let ((fn (format "%s/%s.go" (projectile-project-root) file)))
    (split-window-below)
    (find-file fn)
    (windmove-right)
    (split-window-below)
    (find-file (th/go-alternate-file fn))
    (balance-windows)))

(defun th/go-test-file-p (&optional fn)
  "Returns boolean if the file name given is a test file"
  (s-contains? "_test.go"
               (if (not fn)
                   (buffer-file-name)
                 fn)))

(defun th/go-get-test-above ()
  "Gets the name of the test above point"
  (save-excursion
    (re-search-backward "^func \\(Test\\|Example\\)" nil t)
    (forward-word 2)
    (thing-at-point 'symbol t)))

(defvar th/go-last-single-test "go test" "The last single test command that was run")

(defun th/go-single-test ()
  "If in test file, run the test above point. If not, run the last run test."
  (interactive)
  (projectile-save-project-buffers)
  (when (th/go-test-file-p)
    (setq th/go-single-test (format "go test -tags develop -v -run %s" (th/go-get-test-above))))
  (compile th/go-single-test))

(defun th/go-select-type-signature ()
  "Make a selection of what type to attach a new func to"
  (save-excursion
    (let* ((empty "<none>")
           (type (completing-read
                  "Type: "
                  (append (list empty)
                          (go--get-types)))))
      (if (not (s-equals? type empty))
          (format
           " %s "
           (go--convert-type-name-to-receiver
            (car (s-split " " type))))
        " "))))


(defun th/go-benchmark ()
  (interactive)
  (projectile-save-project-buffers)
  (if (f-exists-p (concat projectile-project-root "Makefile"))
      (compile "make benchmark")
    (compile "go test -v -bench=.")))


(defun th/go-lint ()
  (interactive)
  (projectile-save-project-buffers)
  (compile "make lint"))


(defun th/go-race ()
  (interactive)
  (projectile-save-project-buffers)
  (compile "make race"))


(defun th/go-server-compile ()
  (interactive)
  (projectile-save-project-buffers)
  ;; The server compile command should run in the root
  (with-current-buffer (th/go-main-file-buffer)
    (compile "make drunkenfall")))

(defun th/go-main-file ()
  "Returns the main go file of the project"
  (let ((path (projectile-project-root)))
    (concat path (format "%s.go" (f-base path)))))

(defun th/go-main-file-buffer ()
  "Returns the buffer of the main go file of the project"
  (get-file-buffer (th/go-main-file)))

(defun th/go-modules ()
  "Get the paths to all modules found in the project"
  (let ((path (projectile-project-root)))
    (cdr ;; Remove the ./
     (-uniq
      (-map
       (lambda (fn)
         ;; Add ./ so that go considers the modules as local and not remote
         (concat "./" (f-dirname fn)))
       (-filter
        ;; Return a list with all go files
        (lambda (fn) (s-contains? ".go" fn))
        (projectile-current-project-files)))))))




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

(defvar th/go-server-args nil "Argument to go server start")

(defun th/go-server-start (args)
  "Start the server for the app"
  (interactive
   (list (completing-read "server args: " '("server" ""))))
  (let* ((root (projectile-project-root))
         (name (f-base root))
         (procname (format "%s-server" name))
         (buffer (format "*%s*" procname))
         (default-directory root))

    ;; If the server is already running, stop it; effectively making this a restart.
    (when (get-process procname)
      (th/go-server-stop))

    (setq th/go-server-args args)

    (start-process
     procname
     buffer
     (concat root name)
     th/go-server-args)

    (with-current-buffer buffer
      (goto-char (point-max)))))

(defun th/go-server-stop ()
  "Stop the server for the app"
  (interactive)
  (let* ((name (f-base (projectile-project-root))))
    (delete-process
     (format "*%s-server*" name))
    (message "Stopped %s server" name)))

(defun th/go-server-buffer ()
  "Visit the server output buffer"
  (interactive)
  (let* ((name (f-base (projectile-project-root))))
    (switch-to-buffer (format "*%s-server*" name))))

(defun th/start-drunkenfall ()
  (interactive)

  (let* ((dir th/df)
         (default-directory dir))
    (find-file (concat dir "drunkenfall.go"))

    (enved-load dir)

    (th/go-server-start "server")
    (th/go-server-buffer)

    (start-process-shell-command
     "caddy" "*caddy*" "caddy")

    (compile "make drunkenfall")

    (split-window-below)
    (windmove-down)

    (find-file (concat dir "js/package.json"))
    (npm-mode--exec-process "npm-run" (format "npm run %s" "dev"))

    (balance-windows)))

(provide 'th-golang)
