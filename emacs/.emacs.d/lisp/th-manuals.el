(defun counsel-tldr ()
  "Search https://github.com/tldr-pages/tldr."
  (interactive)
  (let* ((default-directory "~/git/tldr")
         (cands (split-string
                 (shell-command-to-string
                  "git ls-files --full-name -- pages/")
                 nil t)))
    (ivy-read "Topic: " cands
              :action #'find-file
              :caller 'counsel-tldr)))

(provide 'th-manuals)
