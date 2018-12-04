(defvar whitespace-mode nil)
(defhydra th/toggle-hydra (:color pink)
   "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_c_ firestarter-mode:  %`firestarter-mode
_f_ auto-fill-mode:    %`auto-fill-function
_h_ highlight-symbol:  %`highlight-symbol-mode
_i_ fill-indicator:    %`fci-mode
_l_ linum-mode:        %`linum-mode
_r_ rainbow-idents:    %`rainbow-identifiers-mode
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode

"
   ;; Toggles
   ("a" abbrev-mode nil)
   ("d" toggle-debug-on-error nil)
   ("c" firestarter-mode nil)
   ("f" th/toggle-auto-fill nil)
   ("h" highlight-symbol-mode nil)
   ("i" fci-mode nil)
   ("l" linum-mode nil)
   ("r" rainbow-identifiers-mode nil)
   ("t" toggle-truncate-lines nil)
   ("w" whitespace-mode nil)

   ("p" ivy-pass "password" :exit t)

   ("C-f" th/font-hydra/body "font-hydra" :exit t)
   ("C-s" th/yas-hydra/body "yas-hydra" :exit t)

   ("q" nil))

(defun th/toggle-auto-fill ()
  "Toggles auto fill and FCI at the same time"
  (interactive)
  (let ((arg (if auto-fill-function -1 1)))
    (auto-fill-mode arg)
    (fci-mode arg)))

(global-set-key (kbd "C-c C-v") 'th/toggle-hydra/body)

(defun th/iosevka (size)
  (set-frame-font (format "Iosevka-%s" size))
  (telephone-line-mode 1))

(defhydra th/font-hydra (:color amaranth)
  "Fonts"
  ("d" (th/iosevka 10))
  ("f" (th/iosevka 13))
  ("h" (th/iosevka 17))
  ("j" (th/iosevka 20))
  ("k" (th/iosevka 22))
  ("l" (th/iosevka 24))
  ("i" (describe-char (point)) "font information" :exit t)
  ("q" nil))

(defhydra th/exec-hydra (:foreign-keys warn :exit t :columns 5)
  "Execution"
  ("C-c" (projectile-switch-project-by-name "~/src/github.com/thiderman/dotfiles") "config")
  ("d" (find-file "/ssh:di:") "dragonisle")
  ("C-d" daemons "daemons")
  ("f" hydra-flycheck/body "flycheck")
  ("e" enved "enved")
  ("M-e" enved-load "Load 12FA env")
  ("h" hydra-helpful/body "helpful")
  ("g" customize-group "customize")
  ("m" th/quickmajor "major-mode")
  ("n" th/toggle-minor-mode "minor-mode")
  ("p" list-processes "processes")
  ("M-p" proced "proced")

  ("s" th/smerge-hydra/body "smerge")
  ("x" th/hexrgb-hydra/body "hexrgb")
  ("C-q" (save-buffers-kill-emacs t) "exit emacs")
  ("q" nil))

(global-set-key (kbd "s-SPC") 'th/exec-hydra/body)
(global-set-key (kbd "C-x C-C") 'th/exec-hydra/body)

(defhydra th/files-hydra (:color teal :idle 0.5)
  "Files"
  ("f" counsel-projectile-find-file "project files")
  ("b" th/other-files-same-base "base")
  ("s" th/other-files "suffix")
  ("e" th/browse-extensions "extensions")

  ("m" (th/other-files "Makefile") "makefiles")
  ("r" (th/other-files ".http") "rest")
  ("d" (th/other-files "Dockerfile") "docker")
  ("c" (th/other-files "docker-compose") "docker-compose")
  ("i" (th/other-files "gitlab-ci") "gitlab")
  ("t" (th/other-files "test") "test"))

(global-set-key (kbd "C-x f") 'th/files-hydra/body)

(provide 'th-hydra)
