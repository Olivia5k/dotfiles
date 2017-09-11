(defhydra th/util-hydra (:foreign-keys warn :exit t)
  "Util"
  ("s" th/yas-hydra/body "yas")
  ("a" auto-fill-mode "Auto fill")
  ("d" th/toggle-debug "debug")
  ("f" th/font-hydra/body "font-hydra")
  ("g" customize-group "customize")
  ("M-f" fci-mode "Fill column")
  ("h" highlight-symbol-mode "Highlight symbol" :exit nil)
  ("j" text-scale-decrease "Font -" :exit nil)
  ("k" text-scale-increase "Font +" :exit nil)
  ("l" linum-mode "Line numbers" :exit nil)
  ("m" th/quickmajor "major-mode")
  ("n" th/toggle-minor-mode "minor-mode")
  ("r" rainbow-identifiers-mode "Rainbow identifiers")
  ("t" toggle-truncate-lines "Truncate lines" :exit nil)
  ("q" nil))

(global-set-key (kbd "s-c") 'th/util-hydra/body)

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
  ("C-c" (projectile-switch-project-by-name "~/src/github.com/thiderman/dotfiles") "config")
  ("e" elfeed "elfeed")
  ("f" hydra-flycheck/body "flycheck")
  ("M-e" th/load-env "Load 12FA env")
  ("h" hydra-helpful/body "helpful")
  ("g" th/git-hydra/body "git")
  ("p" th/package-hydra/body "package")
  ("M-p" proced "proced")
  ("s" th/smerge-hydra/body "smerge")
  ("t" counsel-tldr "tldr")
  ("x" th/hexrgb-hydra/body "hexrgb")
  ("C-q" (save-buffers-kill-emacs t) "exit emacs")
  ("q" nil))

(global-set-key (kbd "s-SPC") 'th/exec-hydra/body)
(global-set-key (kbd "C-x C-C") 'th/exec-hydra/body)

(defhydra th/package-hydra (:foreign-keys warn :exit t)
  "Packages"
  ("p" counsel-package "package")
  ("r" package-refresh-contents "refresh" :exit nil)
  ("u" paradox-upgrade-packages "upgrade")
  ("l" list-packages "list")
  ("q" nil))

(provide 'th-hydra)
