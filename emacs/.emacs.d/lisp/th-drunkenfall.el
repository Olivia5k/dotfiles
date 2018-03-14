(defun th/start-drunkenfall ()
  (interactive)

  (let* ((dir th/df)
         (default-directory dir)
         (mf (concat dir "Makefile")))
    (find-file mf)

    (enved-load dir)

    ;; (th/go-server-start "server")

    (th/compile-in-buffer mf "npm-run" "*drunkenfall-npm*")
    (th/compile-in-buffer mf "caddy" "*drunkenfall-caddy*")
    (makefile-executor-execute-target mf "drunkenfall")

    (balance-windows)))

(defun th/grab-drunkenfall ()
  (interactive)
  (ssh-agent-add-key "/home/thiderman/.ssh/digitalocean.rsa")

  (copy-file
   "/scp:dropletfall:drunkenfall/data/production.db"
   (concat th/df "data/test.db")
   t)
  (th/go-server-start "server"))

(provide 'th-drunkenfall)
