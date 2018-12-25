(defvar th/df "/home/thiderman/src/gitlab.com/one-eye/drunkenfall/")

(defun th/start-drunkenfall ()
  (interactive)

  (let* ((dir th/df)
         (default-directory dir)
         (mf (concat dir "Makefile")))
    (find-file mf)
    (enved-load dir)

    (makefile-executor-execute-dedicated-buffer mf "npm-start")
    (makefile-executor-execute-dedicated-buffer mf "caddy")
    (makefile-executor-execute-dedicated-buffer mf "postgres")
    (makefile-executor-execute-target mf "drunkenfall-start")

    (switch-to-buffer "*compilation*")

    (th/browser-golden)))

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
        (sqlup-mode 1)))))

(defun th/drunkenfall-term ()
  "Spawn a terminal residing on the main DrunkenFall machine."
  (interactive)
  (ssh-agent-add-key "/home/thiderman/.ssh/digitalocean.rsa")
  (th/exwm-terminal "ssh df -t TERM=xterm-256color tmux new -A -s main"))

(defhydra th/drunkenfall-hydra (:foreign-keys warn :exit t)
  "DrunkenFall"
  ("s-M-d" th/start-drunkenfall "start")
  ("g" (browse-url "https://dev.drunkenfall.com") "browse")
  ("h" (find-file "/ssh:df:/root/src/github.com/drunkenfall/drunkenfall") "host")
  ("t" th/drunkenfall-term "terminal")
  ("RET" th/drunkenfall-term "terminal")
  ("d" th/drunkenfall-db "get db")
  ("p" th/drunkenfall-psql "psql"))

(global-set-key (kbd "s-M-d") 'th/drunkenfall-hydra/body)

(provide 'th-drunkenfall)
