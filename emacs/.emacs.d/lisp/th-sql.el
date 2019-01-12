(require 'sql)

(setq-default sql-product "postgres")

(use-package sqlup-mode
  :hook ((sql-mode . sqlup-mode)
         (sql-mode . yas-minor-mode-on)))

(defadvice sql-execute (after sql-mode-for-table-buffers activate)
  "Sets `sql-mode' in result buffers so they get fontification"
  (sql-mode))

(provide 'th-sql)
