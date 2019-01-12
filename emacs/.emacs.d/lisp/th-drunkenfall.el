(setq th/df "/home/thiderman/src/gitlab.com/one-eye/drunkenfall/")

(defun th/start-drunkenfall ()
  (interactive)

  (let* ((dir th/df)
         (default-directory dir)
         (mf (concat dir "mage.go")))
    (find-file mf)
    (enved-load dir)

    (ssh-agent-add-key "/home/thiderman/.ssh/gitlab.rsa")

    (magefile-executor-execute-dedicated-buffer mf "run:npm")
    (magefile-executor-execute-dedicated-buffer mf "run:proxyDev")
    (magefile-executor-execute-dedicated-buffer mf "run:postgres")
    (magefile-executor-execute-target mf "run:drunkenfall")

    (switch-to-buffer "*compilation*")))

(defun th/drunkenfall-db ()
  (interactive)
  (ssh-agent-add-key "/home/thiderman/.ssh/digitalocean.rsa")

  (copy-file
   "/scp:df:src/github.com/drunkenfall/drunkenfall/data/db.sql"
   (concat th/df "data/db.sql")
   t))

(defun th/drunkenfall-psql ()
  (interactive)
  (let ((buffer (get-buffer "*SQL: drunkenfall-postgres*")))
    (if buffer
        (switch-to-buffer buffer)
      (let ((sql-postgres-program "psql")
            (sql-database "drunkenfall")
            (sql-server "localhost")
            (sql-user "postgres")
            (sql-product "postgres"))
        (sql-postgres "drunkenfall-postgres")
        (sql-set-sqli-buffer-generally)
        (sqlup-mode 1)
        (yas-minor-mode 1)))))

(defun th/drunkenfall-term ()
  "Spawn a terminal residing on the main DrunkenFall machine."
  (interactive)
  (ssh-agent-add-key "/home/thiderman/.ssh/digitalocean.rsa")
  (th/exwm-terminal "ssh df -t TERM=xterm-256color tmux new -A -s main"))

(defhydra th/drunkenfall-hydra (:foreign-keys warn :exit t)
  "DrunkenFall"
  ("d" th/start-drunkenfall "start")
  ("f" (projectile-switch-project-by-name "~/src/gitlab.com/one-eye/drunkenfall") "files")
  ("g" (magit-status "~/src/gitlab.com/one-eye/drunkenfall") "magit")
  ("h" (find-file "/ssh:df:/root/src/github.com/drunkenfall/drunkenfall") "host")
  ("t" th/drunkenfall-term "terminal")
  ("p" th/drunkenfall-psql "psql"))

(global-set-key (kbd "C-c d") 'th/drunkenfall-hydra/body)

(provide 'th-drunkenfall)
