;;; ujelly-theme.el --- Ujelly theme for GNU Emacs 24 (deftheme)

;; Author: Mark Tran <mark.tran@gmail.com>
;; URL: http://github.com/marktran/color-theme-ujelly
;; Version: 1.0.43

;; Inspired by jellybeans: http://www.vim.org/scripts/script.php?script_id=2555.
(deftheme ujelly "The ujelly color theme")

(let ((class '((class color) (min-colors 89)))
      (ujelly-fg                (if (display-graphic-p) "#ffffff" "color-255"))
      (ujelly-bg                (if (display-graphic-p) "#111111" "color-234"))
      (ujelly-blue-0            (if (display-graphic-p) "#8fbfdc" "color-110"))
      (ujelly-blue-1            (if (display-graphic-p) "#97aad3" "color-74"))
      (ujelly-green-0           (if (display-graphic-p) "#99ad6a" "color-107"))
      (ujelly-green-1           (if (display-graphic-p) "#447799" "color-66"))
      (ujelly-green-2           (if (display-graphic-p) "#a8ff60" "color-155"))
      (ujelly-grey-0            (if (display-graphic-p) "#888888" "color-244"))
      (ujelly-grey-1            (if (display-graphic-p) "#7f7f7f" "color-240"))
      (ujelly-grey-2            (if (display-graphic-p) "#151515" "color-237"))
      (ujelly-grey-3            (if (display-graphic-p) "#1c1c1c" "color-235"))
      (ujelly-grey-4            (if (display-graphic-p) "#222222" "color-236"))
      (ujelly-orange-0          (if (display-graphic-p) "#ffb964" "color-215"))
      (ujelly-purple-0          (if (display-graphic-p) "#8197bf" "color-103"))
      (ujelly-purple-1          (if (display-graphic-p) "#474e90" "color-60"))
      (ujelly-purple-2          (if (display-graphic-p) "#cd00cd" "color-96"))
      (ujelly-purple-3          (if (display-graphic-p) "#540063" "color-53"))
      (ujelly-purple-4          (if (display-graphic-p) "#a40073" "color-125"))
      (ujelly-red-0             (if (display-graphic-p) "#97aad2" "color-110"))
      (ujelly-red-1             (if (display-graphic-p) "#dd0093" "color-162"))
      (ujelly-red-2             (if (display-graphic-p) "#de5577" "color-168"))
      (ujelly-red-3             (if (display-graphic-p) "#ff73fd" "color-207"))
      (ujelly-yellow-0          (if (display-graphic-p) "#fad07a" "color-222"))
      (ujelly-yellow-1          (if (display-graphic-p) "#ffff00" "color-179"))
      (ujelly-delimiter-1       (if (display-graphic-p) "#8090a0" "color-103"))
      (ujelly-delimiter-2       (if (display-graphic-p) "#90a080" "color-108"))
      (ujelly-delimiter-3       (if (display-graphic-p) "#a08090" "color-138"))
      (ujelly-delimiter-4       (if (display-graphic-p) "#9080a0" "color-101"))
      (ujelly-delimiter-5       (if (display-graphic-p) "#8090a0" "color-110"))
      (ujelly-delimiter-6       (if (display-graphic-p) "#90a080" "color-182"))
      (ujelly-delimiter-7       (if (display-graphic-p) "#a08090" "color-104"))
      (ujelly-delimiter-8       (if (display-graphic-p) "#9080a0" "color-61"))
      (ujelly-delimiter-9       (if (display-graphic-p) "#9080a0" "color-65"))
      (ujelly-delimiter-10      (if (display-graphic-p) "#9080a0" "color-139"))
      (ujelly-delimiter-11      (if (display-graphic-p) "#9080a0" "color-144"))
      (ujelly-delimiter-12      (if (display-graphic-p) "#9080a0" "color-38"))
      (ujelly-delimiter-13      (if (display-graphic-p) "#9080a0" "color-188"))
      (ujelly-delimiter-14      (if (display-graphic-p) "#9080a0" "color-117"))
      (ujelly-delimiter-15      (if (display-graphic-p) "#9080a0" "color-111"))
      )

      (custom-theme-set-faces
       'ujelly
       `(default                               ((t (:foreground ,ujelly-fg :background ,ujelly-bg))))
       `(compilation-error                     ((t (:foreground ,ujelly-red-0))))
       `(compilation-info                      ((t (:foreground ,ujelly-yellow-0))))
       `(compilation-line-number               ((t (:foreground ,ujelly-grey-0))))
       `(compilation-mode-line-exit            ((t (:foreground ,ujelly-green-0))))
       `(compilation-mode-line-fail            ((t (:foreground ,ujelly-red-0))))
       `(compilation-mode-line-run             ((t (:foreground ,ujelly-yellow-0))))
       `(diredp-date-time                      ((t (:foreground ,ujelly-fg))))
       `(diredp-deletion                       ((t (:foreground ,ujelly-red-0 :background ,ujelly-bg))))
       `(diredp-dir-heading                    ((t (:foreground ,ujelly-yellow-0 :background ,ujelly-bg))))
       `(diredp-dir-priv                       ((t (:foreground ,ujelly-green-2 :background ,ujelly-bg))))
       `(diredp-exec-priv                      ((t (:foreground ,ujelly-fg :background ,ujelly-bg))))
       `(diredp-file-name                      ((t (:foreground ,ujelly-fg))))
       `(diredp-file-suffix                    ((t (:foreground ,ujelly-fg))))
       `(diredp-link-priv                      ((t (:foreground ,ujelly-fg))))
       `(diredp-number                         ((t (:foreground ,ujelly-fg))))
       `(diredp-no-priv                        ((t (:foreground ,ujelly-fg :background ,ujelly-bg))))
       `(diredp-rare-priv                      ((t (:foreground ,ujelly-red-0 :background ,ujelly-bg))))
       `(diredp-read-priv                      ((t (:foreground ,ujelly-fg :background ,ujelly-bg))))
       `(diredp-symlink                        ((t (:foreground ,ujelly-red-3))))
       `(diredp-write-priv                     ((t (:foreground ,ujelly-fg :background ,ujelly-bg))))
       `(emmet-preview-output                  ((t (:background ,ujelly-purple-1))))
       `(erc-notice-face                       ((t (:foreground ,ujelly-yellow-0))))
       `(erc-prompt-face                       ((t (:foreground ,ujelly-fg))))
       `(erc-timestamp-face                    ((t (:foreground ,ujelly-purple-0))))
       `(eshell-prompt                         ((t (:foreground ,ujelly-red-0))))
       `(eshell-ls-directory                   ((t (:weight normal :foreground ,ujelly-green-2))))
       `(eshell-ls-executable                  ((t (:weight normal :foreground ,ujelly-red-0))))
       `(eshell-ls-product                     ((t (:foreground ,ujelly-fg))))
       `(eshell-ls-symlink                     ((t (:weight normal :foreground ,ujelly-purple-2))))
       `(font-lock-builtin-face                ((t (:foreground ,ujelly-orange-0 :bold t))))
       `(font-lock-comment-face                ((t (:foreground ,ujelly-grey-0))))
       `(font-lock-constant-face               ((t (:foreground ,ujelly-green-1 :bold t))))
       `(font-lock-doc-face                    ((t (:foreground ,ujelly-green-0))))
       `(font-lock-function-name-face          ((t (:foreground ,ujelly-yellow-0 :bold t))))
       `(font-lock-keyword-face                ((t (:foreground ,ujelly-purple-0 :bold t))))
       `(font-lock-preprocessor-face           ((t (:foreground ,ujelly-fg))))
       `(font-lock-string-face                 ((t (:foreground ,ujelly-green-0))))
       `(font-lock-type-face                   ((t (:foreground ,ujelly-orange-0 :bold t))))
       `(font-lock-variable-name-face          ((t (:foreground ,ujelly-blue-1 :bold t))))
       `(font-lock-warning-face                ((t (:foreground ,ujelly-red-1))))
       `(font-lock-regexp-grouping-construct   ((t (:foreground ,ujelly-yellow-0 :bold t))))
       `(font-lock-regexp-grouping-backslash   ((t (:foreground ,ujelly-red-0 :bold t))))
       `(fringe                                ((t (:foreground ,ujelly-grey-1 :background ,ujelly-grey-4))))
       `(git-commit-comment-file-face          ((t (:foreground ,ujelly-fg))))
       `(git-commit-comment-heading-face       ((t (:foreground ,ujelly-yellow-0))))
       `(git-commit-summary-face               ((t (:foreground ,ujelly-fg))))
       `(header-line                           ((t (:foreground ,ujelly-fg))))
       `(helm-buffer-size                      ((t (:foreground ,ujelly-fg))))
       `(helm-candidate-number                 ((t (:foreground ,ujelly-fg :background ,ujelly-bg))))
       `(helm-ff-directory                     ((t (:background ,ujelly-bg))))
       `(helm-ff-file                          ((t (:foreground ,ujelly-fg))))
       `(helm-match                            ((t (:foreground ,ujelly-blue-0 :background ,ujelly-bg :bold t))))
       `(helm-selection                        ((t (:foreground ,ujelly-yellow-1 :background ,ujelly-grey-2 :bold t))))
       `(helm-source-header                    ((t (:foreground ,ujelly-red-1 :background ,ujelly-grey-4))))
       `(hl-line                               ((t (:background ,ujelly-grey-4))))
       `(isearch                               ((t (:foreground ,ujelly-fg :background ,ujelly-red-1))))
       `(isearch-fail                          ((t (:background ,ujelly-red-1))))
       `(ido-first-match                       ((t (:foreground ,ujelly-yellow-0))))
       `(ido-only-match                        ((t (:foreground ,ujelly-green-0))))
       `(ido-subdir                            ((t (:foreground ,ujelly-fg))))
       `(ido-virtual                           ((t (:foreground ,ujelly-purple-0))))
       `(lazy-highlight                        ((t (:foreground ,ujelly-red-1 :background nil))))
       `(linum                                 ((t (:foreground ,ujelly-grey-1 :background ,ujelly-grey-3))))
       `(magit-branch                          ((t (:foreground ,ujelly-red-0))))
       `(magit-diff-add                        ((t (:foreground ,ujelly-green-0))))
       `(magit-diff-del                        ((t (:foreground ,ujelly-red-0))))
       `(magit-diff-file-header                ((t (:inherit nil :foreground ,ujelly-red-2))))
       `(magit-diff-hunk-header                ((t (:inherit nil :foreground ,ujelly-yellow-0))))
       `(magit-item-highlight                  ((t (:weight normal :background ,ujelly-grey-4))))
       `(magit-log-author                      ((t (:foreground ,ujelly-fg))))
       `(magit-log-sha1                        ((t (:foreground ,ujelly-red-0))))
       `(magit-log-head-label-local            ((t (:foreground ,ujelly-fg))))
       `(magit-whitespace-warning-face         ((t (:background ,ujelly-red-1))))
       `(match                                 ((t (:background ,ujelly-red-1))))
       `(minibuffer-prompt                     ((t (:foreground ,ujelly-fg))))
       `(mode-line                             ((t (:foreground ,ujelly-fg :background nil))))
       `(mode-line-inactive                    ((t (:foreground ,ujelly-grey-4 :background nil))))
       `(org-checkbox                          ((t (:foreground ,ujelly-green-0))))
       `(org-date                              ((t (:foreground ,ujelly-purple-0))))
       `(org-done                              ((t (:foreground ,ujelly-green-0))))
       `(org-level-1                           ((t (:foreground ,ujelly-red-2))))
       `(org-level-2                           ((t (:foreground ,ujelly-red-0))))
       `(org-level-3                           ((t (:foreground ,ujelly-red-0))))
       `(org-special-keyword                   ((t (:foreground ,ujelly-purple-0))))
       `(org-todo                              ((t (:foreground ,ujelly-yellow-0))))
       `(region                                ((t (:background ,ujelly-purple-1))))

       ;; RainbowDelimiters
       `(rainbow-delimiters-depth-1-face       ((t (:foreground ,ujelly-delimiter-1 ))))
       `(rainbow-delimiters-depth-2-face       ((t (:foreground ,ujelly-delimiter-2 ))))
       `(rainbow-delimiters-depth-3-face       ((t (:foreground ,ujelly-delimiter-3 ))))
       `(rainbow-delimiters-depth-4-face       ((t (:foreground ,ujelly-delimiter-4 ))))
       `(rainbow-delimiters-depth-5-face       ((t (:foreground ,ujelly-delimiter-5 ))))
       `(rainbow-delimiters-depth-6-face       ((t (:foreground ,ujelly-delimiter-6 ))))
       `(rainbow-delimiters-depth-7-face       ((t (:foreground ,ujelly-delimiter-7 ))))
       `(rainbow-delimiters-depth-8-face       ((t (:foreground ,ujelly-delimiter-8 ))))
       `(rainbow-delimiters-depth-9-face       ((t (:foreground ,ujelly-delimiter-9 ))))
       `(rainbow-delimiters-depth-10-face      ((t (:foreground ,ujelly-delimiter-10 ))))
       `(rainbow-delimiters-depth-11-face      ((t (:foreground ,ujelly-delimiter-11 ))))
       `(rainbow-delimiters-depth-12-face      ((t (:foreground ,ujelly-delimiter-12 ))))

       ;; RainbowIdentifiers
       `(rainbow-identifiers-identifier-1      ((t (:foreground ,ujelly-delimiter-1 ))))
       `(rainbow-identifiers-identifier-2      ((t (:foreground ,ujelly-delimiter-2 ))))
       `(rainbow-identifiers-identifier-3      ((t (:foreground ,ujelly-delimiter-3 ))))
       `(rainbow-identifiers-identifier-4      ((t (:foreground ,ujelly-delimiter-4 ))))
       `(rainbow-identifiers-identifier-5      ((t (:foreground ,ujelly-delimiter-5 ))))
       `(rainbow-identifiers-identifier-6      ((t (:foreground ,ujelly-delimiter-6 ))))
       `(rainbow-identifiers-identifier-7      ((t (:foreground ,ujelly-delimiter-7 ))))
       `(rainbow-identifiers-identifier-8      ((t (:foreground ,ujelly-delimiter-8 ))))
       `(rainbow-identifiers-identifier-9      ((t (:foreground ,ujelly-delimiter-9 ))))
       `(rainbow-identifiers-identifier-10     ((t (:foreground ,ujelly-delimiter-10 ))))
       `(rainbow-identifiers-identifier-11     ((t (:foreground ,ujelly-delimiter-11 ))))
       `(rainbow-identifiers-identifier-12     ((t (:foreground ,ujelly-delimiter-12 ))))
       `(rainbow-identifiers-identifier-13     ((t (:foreground ,ujelly-delimiter-13 ))))
       `(rainbow-identifiers-identifier-14     ((t (:foreground ,ujelly-delimiter-14 ))))
       `(rainbow-identifiers-identifier-15     ((t (:foreground ,ujelly-delimiter-15 ))))

       `(smerge-markers                        ((t (:foreground ,ujelly-yellow-0 :background ,ujelly-grey-2))))
       `(smerge-refined-change                 ((t (:foreground ,ujelly-green-0))))
       `(trailing-whitespace                   ((t (:background ,ujelly-red-1))))
       `(web-mode-builtin-face                 ((t (:foreground ,ujelly-blue-0))))
       `(web-mode-html-attr-name-face          ((t (:foreground ,ujelly-purple-0))))
       `(web-mode-html-tag-face                ((t (:foreground ,ujelly-fg))))
       `(web-mode-symbol-face                  ((t (:foreground ,ujelly-green-1))))
       `(whitespace-trailing                   ((t (:background ,ujelly-red-1))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ujelly)

;;; ujelly-theme.el ends here
