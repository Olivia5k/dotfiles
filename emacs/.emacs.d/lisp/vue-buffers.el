(defun vue-buffers-get-section-positions (section &optional buffer)
  "Get the starting and ending positions of the given section"

  (let* ((buffer (or buffer (current-buffer))))
    (save-excursion
      (with-current-buffer buffer
        (beginning-of-buffer)

        (condition-case nil
            (let* ((min
                    ;; The first position is directly after the newline after the section opener
                    (+ (re-search-forward (format "^<%s.*>" section) nil t) 1))
                   ;; The second position is at the beginning of the line with the section closer
                   (max (progn
                          (re-search-forward (format "</%s>" section) nil t)
                          (beginning-of-line)
                          (point))))
              (list min max))

          ;; The primary use case for this is whenever a tag is missing
          (error
           (list -1 -1)))))))

(defun vue-buffers-sections (&optional buffer)
  "Returns the positions of all the sections in BUFFER"
  (let* ((buffer (or buffer (current-buffer))))
    (list (cons 'template (vue-buffers-get-section-positions 'template buffer))
          (cons 'script (vue-buffers-get-section-positions 'script buffer))
          (cons 'style (vue-buffers-get-section-positions 'style buffer)))))

(defun vue-buffers-current-section (&optional buffer)
  "Returns the current section

If we are in a root buffer, it determines the current section
from the tags in the file.
If in a dedicated one, it uses the suffix on the buffer name."
  (interactive)
  (if (vue-buffers-special-p)
      (let ((name (buffer-name (or buffer (current-buffer)))))
        (cond
         ((s-suffix? (format "<%s>" 'template) name)
          'template)
         ((s-suffix? (format "<%s>" 'script) name)
          'script)
         ((s-suffix? (format "<%s>" 'style) name)
          'style)))

    (let* ((buffer (or buffer (current-buffer)))
           (point (point))
           (sections (vue-buffers-sections buffer)))
      (car (-non-nil (mapcar (lambda (s) (when (and (>= point (nth 1 s))
                                               (<= point (nth 2 s)))
                                      (nth 0 s)))
                             sections))))))

;;;###autoload
(defun vue-buffers-switch-buffer (&optional buffer)
  "Switch to a dedicated buffer based on current position, or
switch back to root buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (if (vue-buffers-special-p buffer)
        (switch-to-buffer (vue-buffers-root-buffer buffer))
      (vue-buffers-make-buffer))))

;;;###autoload
(defun vue-buffers-goto-template ()
  "Go to the template tag of the current Vue file"
  (interactive)
  (vue-buffers-make-buffer 'template))

;;;###autoload
(defun vue-buffers-goto-script ()
  "Go to the script tag of the current Vue file"
  (interactive)
  (vue-buffers-make-buffer 'script))

;;;###autoload
(defun vue-buffers-goto-style ()
  "Go to the style tag of the current Vue file"
  (interactive)
  (vue-buffers-make-buffer 'style))

(defun vue-buffers-make-buffer (&optional section buffer)
  "Make a dedicated buffer for a section"
  (interactive)
  (let* ((buffer (or buffer
                     ;; If we are already in a special buffer, we want to make the new special one
                     ;; from the root one
                     (if (vue-buffers-special-p)
                         (vue-buffers-root-buffer)
                       (current-buffer))))
         (section (or section (vue-buffers-current-section buffer)))
         (name (format "%s<%s>" (buffer-name buffer) section))
         (positions (vue-buffers-get-section-positions section buffer))
         (new (get-buffer-create name)))

    (with-current-buffer new
      (erase-buffer)
      (insert-buffer-substring buffer (nth 0 positions) (nth 1 positions))

      (cond
       ((eq section 'template)
        (web-mode))
       ((eq section 'script)
        (js-mode))
       ((eq section 'style)
        (scss-mode)))

      (vue-buffers-mode 1))

    (switch-to-buffer new)))

;;;###autoload
(defun vue-buffers-save-buffer (&optional section buffer)
  "Saves the buffer.

If we're a dedicated buffer, put the content of the buffer in the
corresponding section of the Vue file and save that one."
  (interactive)
  (if (vue-buffers-special-p)
      (let* ((buffer (or buffer (current-buffer)))
             (section (or section (vue-buffers-current-section buffer)))
             (contents (buffer-substring-no-properties (point-min) (point-max))))
        (with-current-buffer (vue-buffers-root-buffer buffer)
          (let* ((pos (vue-buffers-get-section-positions section)))
            ;; Delete the original contents between the tags
            (delete-region (car pos) (cadr pos))
            ;; Go back to where the section starts
            (goto-char (car pos))
            (insert contents)
            (save-buffer))))
    (save-buffer)))

(defun vue-buffers-special-p (&optional buffer)
  "Returns `t' if the current buffer is a dedicated buffer"

  (let* ((name (buffer-name (or buffer (current-buffer)))))
    (or (s-suffix? (format "<%s>" 'template) name)
        (s-suffix? (format "<%s>" 'script) name)
        (s-suffix? (format "<%s>" 'style) name))))

(defun vue-buffers-root-buffer (&optional buffer)
  "Gets the real buffer from a dedicated one"

  (get-buffer
   (s-replace-regexp "<.+>$" "" (buffer-name
                                 (or buffer (current-buffer))))))

(defvar vue-buffers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'vue-buffers-switch-buffer)
    (define-key map (kbd "C-c C-r") 'vue-buffers-switch-buffer)
    (define-key map (kbd "C-c C-m") 'vue-buffers-switch-buffer)
    (define-key map (kbd "C-c C-t") 'vue-buffers-goto-template)
    (define-key map (kbd "C-c C-s") 'vue-buffers-goto-style)
    (define-key map (kbd "C-c C-j") 'vue-buffers-goto-script)
    (define-key map (kbd "C-x C-s") 'vue-buffers-save-buffer)
    map)
  "Keymap for `vue-buffers-mode'.")

(define-minor-mode vue-buffers-mode
  "Tools to handle sections in Vue.js files as separate buffers."
  t "🎇" vue-buffers-mode-map)

(add-hook 'vue-mode-hook 'vue-buffers-mode)

(provide 'vue-buffers)
