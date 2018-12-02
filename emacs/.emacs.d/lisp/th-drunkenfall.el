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
    (makefile-executor-execute-dedicated-buffer mf "postgres")
    (makefile-executor-execute-target mf "drunkenfall-start")

    (switch-to-buffer "*compilation*")

    (th/browser-golden)))

(defun th/drunkenfall-db ()
  (interactive)
  (ssh-agent-add-key "/home/thiderman/.ssh/digitalocean.rsa")

  (copy-file
   "/scp:df:src/github.com/drunkenfall/drunkenfall/data/production.db"
   (concat th/df "data/test.db")
   t))

(defun th/drunkenfall-psql ()
  (interactive)
  (let ((buffer (get-buffer "*SQL: drunkenfall-postgres*")))
    (if buffer
        (switch-to-buffer buffer)
      (let ((sql-postgres-program "psql")
            (sql-database "drunkenfall")
            (sql-server "localhost")
            (sql-user "postgres"))
        (sql-postgres "drunkenfall-postgres")
        (sqlup-mode 1)))))

(defhydra th/drunkenfall-hydra (:foreign-keys warn :exit t)
  "DrunkenFall"
  ("s-M-d" th/start-drunkenfall "start")
  ("g" (browse-url "https://dev.drunkenfall.com") "browse")
  ("h" (find-file "/ssh:df:/root/src/github.com/drunkenfall/drunkenfall") "host")
  ("t" (exwm-execute "kitty -e ssh df") "terminal")
  ("d" th/drunkenfall-db "get db")
  ("p" th/drunkenfall-psql "psql"))

(global-set-key (kbd "s-M-d") 'th/drunkenfall-hydra/body)

(provide 'th-drunkenfall)
