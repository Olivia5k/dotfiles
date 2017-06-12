(use-package elfeed
        :ensure t
        :bind (:map elfeed-search-mode-map
                    ("a" . elfeed-add-feed)
                    ("G" . elfeed-update)
                    ("q" . bjm/elfeed-save-db-and-bury)
                    ("Q" . bjm/elfeed-save-db-and-bury)
                    ("j" . mz/make-and-run-elfeed-hydra)
                    ("J" . mz/make-and-run-elfeed-hydra)
                    ("m" . elfeed-toggle-star)
                    ("M" . elfeed-toggle-star)))

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))
;; (setq elfeed-db-directory "~/Dropbox/shared/elfeeddb")

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

(defun z/hasCap (s)
  ""
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" s)))

(defun z/get-hydra-option-key (s)
  "returns single upper case letter (converted to lower) or first"
  (interactive)
  (let ((loc (z/hasCap s)))
    (if loc
        (downcase (substring s loc (+ loc 1)))
      (substring s 0 1))))

(defun mz/make-elfeed-cats (tags)
  "Returns a list of lists. Each one is line for the hydra configuratio in the form
           (c function hint)"
  (interactive)
  (mapcar (lambda (tag)
            (let* (
                   (tagstring (symbol-name tag))
                   (c (z/get-hydra-option-key tagstring))
                   )
              (list c (append '(elfeed-search-set-filter) (list (format "@6-months-ago +%s" tagstring) ))tagstring  )))
          tags))

(defmacro mz/make-elfeed-hydra ()
  `(defhydra mz/hydra-elfeed ()
     "filter"
     ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
     ("M" elfeed-toggle-star "Mark")
     ("A" (elfeed-search-set-filter "@6-months-ago") "All")
     ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
     ("Q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
     ("q" nil "quit" :color blue)
     ))

(defun mz/make-and-run-elfeed-hydra ()
  ""
  (interactive)
  (mz/make-elfeed-hydra)
  (mz/hydra-elfeed/body))

(provide 'th-elfeed)
