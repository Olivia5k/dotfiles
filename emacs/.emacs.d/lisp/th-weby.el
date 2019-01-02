;; lispy-like editing for web-mode
(require 'web-mode)

(defun weby-special-p ()
  "Returns `t' if standing on an opening tag"
  (and (looking-at "<")
       (looking-back "^\s+")))

(defmacro defweby (name key &rest body)
  (let* ((defun-name (make-symbol (format "weby-%s" name))))

    `(bind-key
      ,key
      (defun ,defun-name (arg)
        (interactive "p")
        (if (weby-special-p)
            (progn
              ,@body)
          (self-insert-command arg)))
      web-mode-map)))

(defweby parent "h" (web-mode-element-parent))
(defweby down "j" (web-mode-element-sibling-next))
(defweby up "k" (web-mode-element-sibling-previous))
(defweby child "l" (web-mode-element-child))

(defweby wrap "w" (web-mode-element-wrap))
(defweby delete "d"
  (web-mode-element-kill arg)
  (whole-line-or-region-kill-region 1)
  (web-mode-element-previous))

(defweby comment ";" (web-mode-comment-or-uncomment))
(defweby splice "M-s" (web-mode-element-vanish arg))

(defweby move-down "J" (web-mode-element-transpose))
;; TODO(thiderman): Doesn't work for the second element for some reason
(defweby move-up "K"
  (web-mode-element-previous)
  (web-mode-element-transpose)
  (web-mode-element-previous))

(defweby end "e"
  (if (looking-at "</")
      (web-mode-element-beginning)
    (web-mode-element-end)
    (th/back-to-indentation-or-bol)))
(defweby beginning "b" (web-mode-element-beginning))

(defweby content "s" (web-mode-element-content-select))

(defweby enter "i"
  (web-mode-element-content-select)
  (deactivate-mark)
  (when (eolp)
    (forward-word 1)
    (backward-word 1)))

(defweby select "q" (avy-goto-word-1 ?<))
(defweby attribute "a"
  (web-mode-attribute-insert)
  (web-mode-element-previous))

(defweby view "v" (recenter))
(defweby clone "c" (web-mode-element-clone arg))
;TODO: Breaks the web-mode binding when in mmm
;; (defweby fold "f" (web-mode-fold-or-unfold))

(provide 'th-weby)
