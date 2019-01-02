(use-package vue-mode
  :init
  (require 'js)
  (require 'css-mode)

  ;; Interestingly enough, the bindings that are vue specific needs to
  ;; be in all the maps.
  ;; TODO(thiderman): Improve on this.
  :bind (("C-c v" . th/vue-switcher)
         :map vue-mode-map
         ("C-c f" . th/vue-goto)
         ("C-c s" . th/vue-toggle-style-scope)
         ("C-c C-n" . th/vue-next-section)
         ("C-c C-p" . th/vue-prev-section)
         :map vue-html-mode-map
         ("C-c f" . th/vue-goto)
         ("C-c C-n" . th/vue-next-section)
         ("C-c C-p" . th/vue-prev-section)
         :map js-mode-map
         ("C-c f" . th/vue-goto)
         ("C-c C-n" . th/vue-next-section)
         ("C-c C-p" . th/vue-prev-section)
         :map css-mode-map
         ("C-c f" . th/vue-goto)
         ("C-c C-n" . th/vue-next-section)
         ("C-c C-p" . th/vue-prev-section)
         ("C-c s" . th/vue-toggle-style-scope))

  :mode "\\.vue\\'"
  :config
  (setq vue-modes
        '((:type template :name nil :mode web-mode)
          (:type template :name html :mode web-mode)
          (:type script :name nil :mode js-mode)
          (:type script :name js :mode js-mode)
          (:type script :name es6 :mode js-mode)
          (:type script :name babel :mode js-mode)
          (:type script :name typescript :mode typescript-mode)
          (:type style :name nil :mode css-mode)
          (:type style :name css :mode css-mode)
          (:type style :name stylus :mode stylus-mode)
          (:type style :name less :mode less-css-mode)
          (:type style :name scss :mode css-mode)
          (:type style :name sass :mode ssass-mode)))

  (setq css-indent-offset 2)
  (setq mmm-submode-decoration-level 0)
  (add-hook 'vue-mode-hook 'th/disable-semantic)
  (add-hook 'vue-mode-hook 'th/vue-auto-yas)
  (add-hook 'vue-mode-hook 'th/vue-auto-template-insert))

(defun th/disable-semantic ()
  (semantic-mode -1))

(defun th/vue-auto-yas ()
  (yas-minor-mode 1))

(defun th/vue-switcher ()
  (interactive)
  (th/other-files-suffix "vue"))

(defun th/vue-goto (kind)
  "Go to the vue section specified by `kind'.

Insert it if it does not exist."
  (interactive
   (list
    (completing-read "vue section: " '(template script style) nil t)))

  (beginning-of-buffer)
  (if (ignore-errors (re-search-forward (format "^<%s" kind)))
      ;; Found the section - go to the beginning of the line
      (beginning-of-line)
    ;; The section was not found. Add it.
    (end-of-buffer)
    (newline (if (and (bolp) (eolp)) 1 2))
    (insert kind)
    (when (string= kind "style")
      (insert "[lang=scss]"))
    (emmet-expand-line nil)
    (newline 1 t)
    (when (string= kind "style")
      (th/vue-toggle-style-scope))))

(defun th/vue-toggle-style-scope ()
  "Add or remove the 'scoped' keyword to the <style> tag."
  (interactive)
  (save-excursion
    (th/vue-goto "style")
    (forward-word 1)
    (when (looking-at " lang")
      (forward-word 2)
      (forward-char 1))
    (if (looking-at " *>")
        (insert " scoped")
      (kill-word 1))))

(defun th/vue-auto-template-insert ()
  "Insert the template block when `vue-mode' is enabled in an empty buffer."
  (when (= (buffer-size) 0)
    (dolist (tag '("template" "div"))
      (insert tag)
      (emmet-expand-line nil)
      (newline-and-indent))))

(defun th/vue-goto-section (search-func)
  "Search backwards or forwards for the beginning of a vue section"

  (let ((success (ignore-errors (funcall search-func "^<[^/]"))))
    (beginning-of-line)
    success))

(defun th/vue-prev-section ()
  "Go to the previous vue section."
  (interactive)
  (th/vue-goto-section 're-search-backward))

(defun th/vue-next-section ()
  "Go to the next vue section."
  (interactive)
  (when (looking-at "^<")
    ;; If we're already at the beginning of a tag, we need to move
    ;; forward to be able to search past it.
    (forward-char 1))

  (when (not (th/vue-goto-section 're-search-forward))
    ;; If it didn't work, check if that is because the next tag doesn't
    ;; exist yet. If such is the case then create it.
    (let ((l (length (th/vue-tags-in-buffer))))
      (cond
       ((= l 0)
        (th/vue-goto "template"))
       ((= l 1)
        (th/vue-goto "script"))
       ((= l 2)
        (th/vue-goto "style"))))))

(defun th/vue-tags-in-buffer ()
  "Returns the current root tags in the buffer"

  (-filter
   (lambda (s) (and (s-prefix? "<" s) (not (s-prefix? "</" s))))
   (s-split "\n" (buffer-substring-no-properties (point-min) (point-max)))))

(use-package web-mode
  :after vue-mode
  :bind (:map web-mode-map
              ("M-a" . web-mode-element-beginning)
              ("M-e" . web-mode-element-end)
              ("M-h" . web-mode-mark-and-expand))

  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package emmet-mode
  :after vue-mode
  :bind (:map emmet-mode-keymap
              ("C-j" . th/emmet-vue-expand))
  :init
  (setq emmet-indentation 2)
  (setq emmet-insert-flash-time 0.01)

  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; Enable Emmet's css abbreviation.
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'vue-mode-hook  'emmet-mode))

(defun th/emmet-vue-expand ()
  "Use `emmet-expand-line' and set `emmet-use-css-transform' depending on the major mode."
  (interactive)
  (let ((emmet-use-css-transform (derived-mode-p 'css-mode)))
    (emmet-expand-line nil)))

(setq httpd-port 8001)

(use-package npm-mode
  :config
  (add-hook 'json-mode-hook 'npm-mode))

(defun semantic-idle-scheduler-function (&rest))

(provide 'th-web)
