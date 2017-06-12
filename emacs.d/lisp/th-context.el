(defun th/context-execute ()
  "Context aware execution of what's under point"

  (interactive)
  (let ((symbol (th/context-get-whitespace-word)))
    (message "Executing: <%s>" symbol)

    (cond
     ;; JIRA tickets
     ((s-matches? "[A-Z][A-Z]+-[0-9]+" symbol)
      (th/context-visit-jira symbol))

     ;; URLs
     ((s-matches? "[a-z]+://" symbol)
      (browse-url symbol)))

    ))

(defun th/context-get-whitespace-word ()
  "Get the whitespace delimited word under point."

  (let ((start (save-excursion (re-search-backward "\\(^\\| \\)")))
        (end (save-excursion (re-search-forward "\\( \\|$\\)"))))
    (s-trim (buffer-substring start end))))

(defun th/context-visit-jira (ticket)
  "Visit the JIRA page"
  (interactive)
  (browse-url (concat th/context-jira-root "/browse/" ticket)))

(defvar th/context-jira-root "https://jira.spotify.net" "Root URL for JIRA")

(global-set-key (kbd "C-x RET") 'th/context-execute)

(provide 'th-context)
