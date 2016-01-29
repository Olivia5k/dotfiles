(require 'url)
(require 'json)
(require 'helm)

(defun spearch-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url "http://search.spotify.net/api/search/services/rest/index/Everything%20query/search/field/search")
;;  (let ((a-url "http://127.0.0.1:8081/search/")
        (url-automatic-caching t)
        (url-request-data (format "{\"query\": \"%s\"}" search-term))
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json;charset=UTF-8"))))
    (with-current-buffer
        (url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun spearch-field-value (name fields)
  (dolist (elt (mapcar 'identity fields) url)
    (if (string-equal (cdr (assoc 'fieldName elt)) name)
        (setq url (aref (cdr (assoc 'values elt)) 0)))))

(defun spearch-search-pattern (pattern)
 (mapcar (lambda (item)
           (let ((snippets (cdr (assoc 'snippets item)))
                 (fields (cdr (assoc 'fields item))))
             (list (cons 'url (spearch-field-value "url" fields))
                   (cons 'title (spearch-field-value "title" snippets))
                   (cons 'content (spearch-field-value "content" snippets)))))
         (cdr (assoc 'documents (spearch-search pattern)))))

(defun spearch-format-item (item)
  (concat
   (propertize (format "%s" (cdr (assoc 'title item)))
               'face 'font-lock-function-name-face)
   (propertize (format " %s" (cdr (assoc 'content item)))
               'face 'font-lock-doc-face)))

(defun spearch-search-formatted (pattern)
  (mapcar (lambda (item)
            (cons (spearch-format-item item) item))
          (spearch-search-pattern pattern)))

(defun helm-spearch-search ()
  (let ((result (spearch-search-formatted helm-pattern)))
    result))

(defvar helm-source-spearch
      '((name . "Spotify Search")
        (candidates . helm-spearch-search)
        (multiline)
        (action . (("open in browser" . (lambda (candidate) (browse-url (cdr (assoc 'url candidate)))))
                   ("open in emacs" . (lambda (candidate) (eww (cdr (assoc 'url candidate)))))))
        (requires-pattern . 2)
        (volatile)))

;;;###autoload
(defun helm-spearch ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-spearch)
        :prompt: "Spotify: "
        :buffer "*helm-spearch*"))

(provide 'helm-spearch)
