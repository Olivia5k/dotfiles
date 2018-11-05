(use-package go-mode
  :bind
  (:map go-mode-map
        ("C-c C-j" . godef-jump-other-window)
        ("M-."     . godef-jump)

        ("C-c C-f" . go-goto-hydra/body)
        ("M-m"     . go-goto-hydra/body)
        ("C-c i"   . go-goto-imports)
        ("C-c e"   . th/go-run-main)

        ("C-c c"   . th/go-coverage-toggle)
        ("C-c a"   . ff-find-other-file)
        ("C-M-x"   . th/go-single-test)
        ("C-c C-c" . makefile-executor-execute-last)

        ("DEL"     . go-delete-backward-char)
        ("C-d"     . go-delete-char)

        ("C-c r"   . go-rename)
        ("C-c C-m" . go-refactor-map)
        ("C-M-d"   . go-refactor-declaration-colon)
        ("M-r"     . go-refactor-unwrap)
        ("C-c l"   . go-refactor-toggle-nolint))

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
  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)
          ("test-run" "t.Run(\"*\\(.*\\)\", func" 1)))

  ;; This needs to be set in the hook rather than in the :map because
  ;; the go-guru mode map will override it otherwise.
  (define-key go-mode-map (kbd "C-c C-o") 'th/go-guru/body)

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
  (interactive)
  (let ((fn "coverage.out"))
    (if (f-exists? fn)
        (go-coverage fn)
      (error "No coverage file found in PWD"))))

(defun th/go-test-file-p (&optional fn)
  "Returns boolean if the file name given is a test file"
  (s-contains? "_test.go" (or fn (buffer-file-name))))

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
    (setq th/go-single-test
          (format "go test -tags develop -v -run %s -coverprofile=coverage.out" (th/go-get-test-above))))
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

(defun th/go-run-main ()
  (interactive)
  (compile "go run main.go"))

(require 'th-golang-docstring)
(require 'th-golang-refactor)
(require 'th-drunkenfall)

(provide 'th-golang)
