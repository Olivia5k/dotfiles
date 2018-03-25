(use-package gitconfig-mode)
(use-package gitignore-mode)
(use-package git-commit
  :init
  (setq git-commit-summary-max-length 79))


;; Move back and forth between commits <3
(use-package git-timemachine)


;; List and edit gists on github.com <3
(use-package gist)


;; <3 <3 <3
(use-package magit
  :bind (("C-x g" . th/magit-status)
         ("s-g" . th/magit-status)
         :map magit-status-mode-map
         ("q"   . magit-mode-bury-buffer)
         ;; In certain modes we want to just kill the window, not magit entirely
         :map magit-process-mode-map
         ("q"   . delete-window))

  :init
  (defun th/magit-status ()
    (interactive)
    (save-some-buffers t)
    (magit-status))

  (setq magit-save-some-buffers 'dontask)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-no-confirm '(reverse trash delete discard drop-stashes stage-all-changes))

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defadvice magit-mode-bury-buffer (after magit-restore-screen activate)
    "Restores the previous window configuration and kills the magit buffer"
    (jump-to-register :magit-fullscreen))

  (defadvice magit-push-dwim (before ssh-agent-with-magit-push activate)
    (th/auto-add-ssh-key))
  (defadvice magit-fetch (before ssh-agent-with-magit-fetch activate)
    (th/auto-add-ssh-key)))

(defun th/auto-add-ssh-key ()
  "Try to add an ssh-key based on the remote"
  (let* ((url (car (magit-config-get-from-cached-list "remote.origin.url")))
         (key (cond
               ((s-contains? "lab.unomaly.com" url)
                "unomaly")
               ((s-contains? "gitlab.com" url)
                "unomaly-gitlab"))))
    (when key
      (ssh-agent-add-key
       (concat (getenv "HOME")
               (format "/.ssh/%s.rsa" key))))))


(use-package git-gutter-fringe+
  :diminish (git-gutter+-mode " gut")
  :config
  (add-hook 'prog-mode-hook 'git-gutter+-mode))


(defhydra th/git-hydra ()
  "git"
  ("n" git-gutter+-next-hunk "next")
  ("p" git-gutter+-previous-hunk "prev")
  ("s" git-gutter+-stage-hunks "stage")
  ("r" git-gutter+-revert-hunk "revert")
  ("g" magit-status "magit" :exit t)
  ("b" magit-blame "blame" :exit t)
  ("l" magit-log-buffer-file "log" :exit t)
  ("m" git-gutter+-mode "gutter-mode")
  ("t" git-timemachine "timemachine" :exit t)
  ("s" th/smerge-hydra/body "smerge" :exit t)
  ("RET" magithub-browse "repo" :exit t)
  ("SPC" git-gutter+-show-hunk-inline-at-point "show")
  ("q" nil))

(global-set-key (kbd "C-x C-g") 'th/git-hydra/body)

(defhydra th/smerge-hydra (:foreign-keys warn
                           :pre (smerge-mode 1))
  "smerge"
  ("n" smerge-next "next")
  ("p" smerge-prev "prev")
  ("a" smerge-keep-all "all")
  ("c" smerge-keep-current "current")
  ("RET" smerge-keep-current "current")
  ("o" smerge-keep-other "other")
  ("DEL" smerge-keep-other "other")
  ("m" smerge-keep-mine "mine")
  ("SPC" smerge-keep-mine "mine")
  ("b" smerge-keep-base "base")
  ("q" (smerge-mode -1) :exit t))

;; TODO(thiderman) This would be super nice to have activate immediately when
;; you enter a buffer that has smerge!
;; (add-hook 'smerge-mode-hook 'th/smerge-hydra/body)


(provide 'th-git)
