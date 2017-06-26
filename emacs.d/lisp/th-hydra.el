(defhydra th/util-hydra (:foreign-keys warn)
  "Util"
  ("s" th/yas-hydra/body "yas" :exit t)
  ("a" auto-fill-mode "Auto fill")
  ("f" th/font-hydra/body "font-hydra" :exit t)
  ("M-f" fci-mode "Fill column")
  ("h" highlight-symbol-mode "Highlight symbol")
  ("j" text-scale-decrease "Font -")
  ("k" text-scale-increase "Font +")
  ("l" linum-mode "Line numbers")
  ("r" rainbow-identifiers-mode "Rainbow identifiers")
  ("t" toggle-truncate-lines "Truncate lines")
  ("q" nil))

(global-set-key (kbd "C-x c") 'th/util-hydra/body)

(defun th/iosevka (size)
  (set-frame-font (format "Iosevka-%s" size)))

(defhydra th/font-hydra ()
  "Fonts"
  ("d" (th/iosevka 10))
  ("f" (th/iosevka 13))
  ("h" (th/iosevka 17))
  ("j" (th/iosevka 20))
  ("k" (th/iosevka 22))
  ("l" (th/iosevka 24))
  ("i" (describe-char (point)) "font information" :exit t)
  ("q" nil))


(defhydra th/exec-hydra (:foreign-keys warn :exit t)
  "Execution"
  ("C-c" (projectile-switch-project-by-name "~/git/dotfiles") "config")
  ("e" elfeed "elfeed")
  ("M-e" th/load-env "Load 12FA env")
  ("h" (counsel-find-file "/") "hosts")
  ("g" th/git-hydra/body "git")
  ("m" th/quickmajor "major-mode")
  ("n" th/toggle-minor-mode "minor-mode")
  ("p" th/package-hydra/body "package")
  ("M-p" proced "proced")
  ("s" th/smerge-hydra/body "smerge")
  ("t" counsel-tldr "tldr")
  ("x" th/hexrgb-hydra/body "hexrgb")
  ("C-q" save-buffers-kill-emacs "exit emacs")
  ("q" nil))

(global-set-key (kbd "C-x C-c") 'th/exec-hydra/body)

(defhydra th/package-hydra (:foreign-keys warn :exit t)
  "Packages"
  ("p" counsel-package "package")
  ("r" package-refresh-contents "refresh" :exit nil)
  ("u" paradox-upgrade-packages "upgrade")
  ("l" list-packages "list")
  ("q" nil))

(provide 'th-hydra)
