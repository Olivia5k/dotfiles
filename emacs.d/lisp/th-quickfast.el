(defun list-major-modes (&optional modes)
  "Returns list of potential major mode names (without the final -mode).
Note, that this is guess work."
  ;; https://stackoverflow.com/a/19165202/983746
  (interactive)
  (let (l)
    (mapatoms
     #'(lambda (f) (and
               (commandp f)
               (string-match "-mode$" (symbol-name f))
               (if modes
                   (let ((parent f))
                     (while (and (not (memq parent modes))
                                 (setq parent (get parent 'derived-mode-parent))))
                     parent)
                 t)
               ;; auto-loaded
               (or (and (autoloadp (symbol-function f))
                        (let ((doc (documentation f)))
                          (when doc
                            (and
                             (let ((docSplit (help-split-fundoc doc f)))
                               (and docSplit ;; car is argument list
                                    (null (cdr (read (car docSplit)))))) ;; major mode starters have no arguments
                             (if (string-match "[mM]inor" doc) ;; If the doc contains "minor"...
                                 (string-match "[mM]ajor" doc) ;; it should also contain "major".
                               t) ;; else we cannot decide therefrom
                             ))))
                   (null (help-function-arglist f)))
               (setq l (cons (substring (symbol-name f) 0 -5) l)))))
    (when (called-interactively-p 'any)
      (with-current-buffer (get-buffer-create "*Major Modes*")
        (clear-buffer-delete)
        (let ((standard-output (current-buffer)))
          (display-completion-list l)
          (display-buffer (current-buffer)))))
    (sort l 'string<)))

(defun th/quickmajor (p)
  "Create a quick buffer set in a major mode.

By default only presents a selection of modes derived from `prog-mode'
or `text-mode'.  With universal argument, all major modes are shown."
  (interactive "P")
  (let* ((derived (unless p '(prog-mode text-mode)))
         (modes (list-major-modes derived))
         (mode (completing-read "Create major mode buffer: " modes nil t)))
    (split-window-sensibly)
    (switch-to-buffer (format "*%s*" mode))
    (funcall (intern (format "%s-mode" mode)))))

(defun list-minor-modes ()
  "Returns list of currently active minor modes."
  (interactive)
  ;; https://stackoverflow.com/a/41709866/983746
  (sort
   (delq nil
         (mapcar
          (lambda (x)
            (let ((car-x (car x)))
              (when (and (symbolp car-x) (symbol-value car-x))
                (car x))))
          minor-mode-alist))
   'string<))

(defun th/toggle-minor-mode ()
  "Presents a selection of minor modes as to toggle one of them."
  (interactive)
  (let* ((active-modes (list-minor-modes))
         (modes (-map (lambda (x) (format "%s%s" (if (-contains? active-modes x) "-" "+") x))
                      minor-mode-list))
         (item (completing-read "minor mode (enable +mode or disable -mode): "
                                (sort modes 'string<) nil t "^- "))
         (enable (if (s-prefix? "+" item) 1 -1))
         (mode (substring item 1)))
    (funcall (intern mode) enable)
    (message "%s %s" mode (if enable "enabled" "disabled"))))



;;;###autoload
(defun th/toggle-minibuffer-prefix ()
  "Toggle between \"^+\" and \"^-\" in minibuffers."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\^")
      (forward-char 1)
      (cond
       ((looking-at "+")
        (delete-char 1)
        (insert "-"))
       ((looking-at "-")
        (delete-char 1)
        (insert "+"))))))

(defun th/toggle-minibuffer-prefix-set-key ()
  (local-set-key (kbd "C-c C-c") #'th/toggle-minibuffer-prefix))

(add-hook 'minibuffer-setup-hook #'th/toggle-minibuffer-prefix-set-key)


(provide 'th-quickfast)
