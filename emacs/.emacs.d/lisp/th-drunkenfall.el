(defvar th/df "/home/thiderman/src/github.com/drunkenfall/drunkenfall/")

(defun th/start-drunkenfall ()
  (interactive)

  (let* ((dir th/df)
         (default-directory dir)
         (mf (concat dir "Makefile")))
    (find-file mf)
    (enved-load dir)

    (makefile-executor-execute-dedicated-buffer mf "npm-start")
    (makefile-executor-execute-dedicated-buffer mf "caddy")
    (makefile-executor-execute-target mf "drunkenfall-start")

    (balance-windows)))

(defun th/drunkenfall-db ()
  (interactive)
  (ssh-agent-add-key "/home/thiderman/.ssh/digitalocean.rsa")

  (copy-file
   "/scp:df:src/github.com/drunkenfall/drunkenfall/data/production.db"
   (concat th/df "data/test.db")
   t))

(defhydra th/drunkenfall-hydra (:foreign-keys warn :exit t)
  "DrunkenFall"
  ("s" th/start-drunkenfall "start")
  ("s-M-d" th/start-drunkenfall "start")
  ("g" (browse-url "https://dev.drunkenfall.com") "browse")
  ("d" th/drunkenfall-db "get db"))

(global-set-key (kbd "s-M-d") 'th/drunkenfall-hydra/body)

(provide 'th-drunkenfall)
