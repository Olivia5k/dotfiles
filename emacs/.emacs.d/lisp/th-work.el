(use-package vagrant-tramp)

(defvar th/unomaly-repo "~/src/lab.unomaly.net/unomaly/")

(let ((elisp (concat th/unomaly-repo "elisp/")))
  (when (f-exists? elisp)
    (add-to-list 'load-path elisp)
    (require 'unomaly)))

(provide 'th-work)
