(use-package vagrant-tramp)

(defvar th/unomaly-repo "~/src/lab.unomaly.net/unomaly/")

(when (f-exists? th/unomaly-repo)
  (add-to-list 'load-path (concat th/unomaly-repo "elisp/"))
  (require 'unomaly))

(provide 'th-work)
