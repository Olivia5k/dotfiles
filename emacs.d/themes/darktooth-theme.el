;;; darktooth-theme.el --- colour theme for Emacs, a remix of gruvbox

;; Copyright (c) 2015 Jasonm23

;; Authors: Jasonm23 <jasonm23@gmail.com>
;; URL: http://github.com/emacsfodder/emacs-theme-darktooth
;; Version: 0.1.9

;;; Commentary:

;; A remix of the excellent theme Gruvbox.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme darktooth-custom "A remix of the excellent theme Gruvbox - custom by thiderman")

(let* ((g (display-graphic-p))
      (darktooth-dark0_hard      (if g "#1D2021" "color-234"))
      (darktooth-dark0           (if g "#282828" "color-235"))
      (darktooth-dark0_soft      (if g "#32302F" "color-236"))
      (darktooth-dark1           (if g "#3C3836" "color-237"))
      (darktooth-dark2           (if g "#504945" "color-239"))
      (darktooth-dark3           (if g "#665C54" "color-241"))
      (darktooth-dark4           (if g "#7C6F64" "color-243"))

      (darktooth-medium          (if g "#928374" "color-245")) ;; or 244

      (darktooth-light0_hard     (if g "#FFFFC8" "color-230"))
      (darktooth-light0          (if g "#FDF4C1" "color-229"))
      (darktooth-light0_soft     (if g "#F4E8BA" "color-228"))
      (darktooth-light1          (if g "#EBDBB2" "color-223"))
      (darktooth-light2          (if g "#D5C4A1" "color-250"))
      (darktooth-light3          (if g "#BDAE93" "color-248"))
      (darktooth-light4          (if g "#A89984" "color-246"))

      (darktooth-bright_red      (if g "#FB4933" "color-167"))
      (darktooth-bright_green    (if g "#B8BB26" "color-142"))
      (darktooth-bright_yellow   (if g "#FABD2F" "color-214"))
      (darktooth-bright_blue     (if g "#83A598" "color-109"))
      (darktooth-bright_purple   (if g "#D3869B" "color-175"))
      (darktooth-bright_aqua     (if g "#8EC07C" "color-108"))
      (darktooth-bright_orange   (if g "color-208" "color-208"))

      ;; neutral, no 256-color code, requested, nice work-around meanwhile
      (darktooth-neutral_red     (if g "#FB4934" "#D75F5F"))
      (darktooth-neutral_green   (if g "#B8BB26" "#AFAF00"))
      (darktooth-neutral_yellow  (if g "#FABD2F" "#FFAF00"))
      (darktooth-neutral_blue    (if g "#83A598" "#87AFAF"))
      (darktooth-neutral_purple  (if g "#D3869B" "#D787AF"))
      (darktooth-neutral_aqua    (if g "#8EC07C" "#87AF87"))
      (darktooth-neutral_orange  (if g "#FF8700" "#FF8700"))

      (darktooth-faded_red       (if g "#9D0006" "color-88"))
      (darktooth-faded_green     (if g "#79740E" "color-100"))
      (darktooth-faded_yellow    (if g "#B57614" "color-136"))
      (darktooth-faded_blue      (if g "#076678" "color-24"))
      (darktooth-faded_purple    (if g "#8F3F71" "color-96"))
      (darktooth-faded_aqua      (if g "#427B58" "color-66"))
      (darktooth-faded_orange    (if g "#AF3A03" "color-130"))

      (darktooth-dark_red       (if g  "#421E1E" "color-52"))
      (darktooth-dark_green     (if g  "#2B280F" "color-22"))
      (darktooth-dark_yellow    (if g  "#4D3B27" "color-3"))
      (darktooth-dark_blue      (if g  "#2B3C44" "color-4"))
      (darktooth-dark_purple    (if g  "#4E3D45" "color-57"))
      (darktooth-dark_aqua      (if g  "#36473A" "color-23"))
      (darktooth-dark_orange    (if g  "#613620" "color-130"))

      (darktooth-delimiter-one   (if g "#5C7E81" "color-66"))
      (darktooth-delimiter-two   (if g "#837486" "color-102"))
      (darktooth-delimiter-three (if g "#9C6F68" "color-94"))
      (darktooth-delimiter-four  (if g "#7B665C" "color-137"))

      (darktooth-white           (if g "#FFFFFF" "white"))
      (darktooth-black           (if g "#000000" "black"))
      (darktooth-sienna          (if g "sienna" "sienna"))
      (darktooth-darkslategray4  (if g "#528B8B" "DarkSlateGray4"))
      (darktooth-lightblue4      (if g "#66999D" "LightBlue4"))
      (darktooth-burlywood4      (if g "#BBAA97" "burlywood4"))
      (darktooth-aquamarine4     (if g "#83A598" "aquamarine4"))
      (darktooth-turquoise4      (if g "#61ACBB" "turquoise4")))

  (custom-theme-set-faces
   'darktooth

   ;; UI
   `(default                                   ((t (:foreground ,darktooth-light0 :background ,darktooth-dark0 ))))
   `(cursor                                    ((t ( :background ,darktooth-light0 ))))

   `(mode-line                                 ((t (:foreground ,darktooth-light1 :background ,darktooth-dark0_hard :box nil ))))
   `(mode-line-inactive                        ((t (:foreground ,darktooth-light4 :background ,darktooth-dark2 :box nil ))))
   `(fringe                                    ((t ( :background ,darktooth-dark1 ))))
   `(linum                                     ((t ( :background ,darktooth-dark0_hard :foreground ,darktooth-dark4 nil))))
   `(hl-line                                   ((t ( :background ,darktooth-dark1 ))))
   `(region                                    ((t ( :background ,darktooth-dark1 ))))
   `(secondary-selection                       ((t ( :background ,darktooth-dark_blue ))))
   `(cua-rectangle                             ((t ( :background ,darktooth-dark_aqua ))))
   `(minibuffer-prompt                         ((t (:foreground ,darktooth-turquoise4 :background ,darktooth-dark0 :bold nil ))))

   ;; Built-in syntax
   `(font-lock-builtin-face                    ((t (:foreground ,darktooth-bright_orange ))))
   `(font-lock-constant-face                   ((t (:foreground ,darktooth-burlywood4 ))))
   `(font-lock-comment-face                    ((t (:foreground ,darktooth-dark4 ))))
   `(font-lock-function-name-face              ((t (:foreground ,darktooth-bright_purple ))))
   `(font-lock-keyword-face                    ((t (:foreground ,darktooth-sienna ))))
   `(font-lock-string-face                     ((t (:foreground ,darktooth-darkslategray4 ))))
   `(font-lock-variable-name-face              ((t (:foreground ,darktooth-aquamarine4 ))))
   `(font-lock-type-face                       ((t (:foreground ,darktooth-lightblue4 ))))
   `(font-lock-warning-face                    ((t (:foreground ,darktooth-neutral_red :bold t ))))
   `(vertical-border                           ((t (:foreground ,darktooth-black :bold t ))))

   ;; avy
   `(avy-background-face                       ((t (:foreground ,darktooth-dark3 ))))
   `(avy-lead-face                             ((t (:background ,darktooth-bright_red :foreground ,darktooth-dark0_hard :weight bold ))))
   `(avy-lead-face-0                           ((t (:background ,darktooth-bright_green :foreground ,darktooth-dark0_hard :weight bold ))))
   `(avy-lead-face-1                           ((t (:background ,darktooth-bright_yellow :foreground ,darktooth-dark0_hard :weight bold ))))
   `(avy-lead-face-2                           ((t (:background ,darktooth-bright_blue :foreground ,darktooth-dark0_hard :weight bold ))))

   ;; whitespace-mode
   `(whitespace-space                          ((t (:foreground ,darktooth-dark4 :background ,darktooth-dark0 ))))
   `(whitespace-hspace                         ((t (:foreground ,darktooth-dark4 :background ,darktooth-dark0 ))))
   `(whitespace-tab                            ((t (:foreground ,darktooth-dark4 :background ,darktooth-dark0 ))))
   `(whitespace-newline                        ((t (:foreground ,darktooth-dark4 :background ,darktooth-dark0 ))))
   `(whitespace-trailing                       ((t (:foreground ,darktooth-neutral_red :background ,darktooth-dark1 ))))
   `(whitespace-line                           ((t (:foreground ,darktooth-neutral_red :background ,darktooth-dark1 ))))
   `(whitespace-space-before-tab               ((t (:foreground ,darktooth-dark4 :background ,darktooth-dark0 ))))
   `(whitespace-indentation                    ((t (:foreground ,darktooth-dark4 :background ,darktooth-dark0 ))))
   `(whitespace-empty                          ((t (:foreground nil :background nil ))))
   `(whitespace-space-after-tab                ((t (:foreground ,darktooth-dark4 :background ,darktooth-dark0 ))))

   ;; RainbowDelimiters
   `(rainbow-delimiters-depth-1-face           ((t (:foreground ,darktooth-delimiter-one ))))
   `(rainbow-delimiters-depth-2-face           ((t (:foreground ,darktooth-delimiter-two ))))
   `(rainbow-delimiters-depth-3-face           ((t (:foreground ,darktooth-delimiter-three ))))
   `(rainbow-delimiters-depth-4-face           ((t (:foreground ,darktooth-delimiter-four ))))
   `(rainbow-delimiters-depth-5-face           ((t (:foreground ,darktooth-delimiter-one ))))
   `(rainbow-delimiters-depth-6-face           ((t (:foreground ,darktooth-delimiter-two ))))
   `(rainbow-delimiters-depth-7-face           ((t (:foreground ,darktooth-delimiter-three ))))
   `(rainbow-delimiters-depth-8-face           ((t (:foreground ,darktooth-delimiter-four ))))
   `(rainbow-delimiters-depth-9-face           ((t (:foreground ,darktooth-delimiter-one ))))
   `(rainbow-delimiters-depth-10-face          ((t (:foreground ,darktooth-delimiter-two ))))
   `(rainbow-delimiters-depth-11-face          ((t (:foreground ,darktooth-delimiter-three ))))
   `(rainbow-delimiters-depth-12-face          ((t (:foreground ,darktooth-delimiter-four ))))
   `(rainbow-delimiters-unmatched-face         ((t (:foreground ,darktooth-light0 :background nil ))))

   ;; RainbowIdentifiers
   `(rainbow-identifiers-identifier-1          ((t (:foreground ,darktooth-delimiter-one ))))
   `(rainbow-identifiers-identifier-2          ((t (:foreground ,darktooth-delimiter-two ))))
   `(rainbow-identifiers-identifier-3          ((t (:foreground ,darktooth-delimiter-three ))))
   `(rainbow-identifiers-identifier-4          ((t (:foreground ,darktooth-delimiter-four ))))
   `(rainbow-identifiers-identifier-5          ((t (:foreground ,darktooth-delimiter-one ))))
   `(rainbow-identifiers-identifier-6          ((t (:foreground ,darktooth-delimiter-two ))))
   `(rainbow-identifiers-identifier-7          ((t (:foreground ,darktooth-delimiter-three ))))
   `(rainbow-identifiers-identifier-8          ((t (:foreground ,darktooth-delimiter-four ))))
   `(rainbow-identifiers-identifier-9          ((t (:foreground ,darktooth-delimiter-one ))))
   `(rainbow-identifiers-identifier-10         ((t (:foreground ,darktooth-delimiter-two ))))
   `(rainbow-identifiers-identifier-11         ((t (:foreground ,darktooth-delimiter-three ))))
   `(rainbow-identifiers-identifier-12         ((t (:foreground ,darktooth-delimiter-four ))))
   `(rainbow-identifiers-identifier-13         ((t (:foreground ,darktooth-delimiter-one ))))
   `(rainbow-identifiers-identifier-14         ((t (:foreground ,darktooth-delimiter-two ))))
   `(rainbow-identifiers-identifier-15         ((t (:foreground ,darktooth-delimiter-three ))))

   ;; linum-relative
   `(linum-relative-current-face               ((t (:foreground ,darktooth-light4 :background ,darktooth-dark1 ))))

   ;; Highlight indentation mode
   `(highlight-indentation-current-column-face ((t ( :background ,darktooth-dark2 ))))
   `(highlight-indentation-face                ((t ( :background ,darktooth-dark1 ))))

   ;; Smartparens
   `(sp-pair-overlay-face                      ((t ( :background ,darktooth-dark2 ))))
   ;;`(sp-wrap-overlay-face                    ((t (:inherit sp-wrap-overlay-face ))))
   ;;`(sp-wrap-tag-overlay-face                ((t (:inherit sp-wrap-overlay-face ))))
   `(sp-show-pair-match-face                   ((t ( :background ,darktooth-dark2 )))) ;; Pair tags highlight
   `(sp-show-pair-mismatch-face                ((t ( :background ,darktooth-neutral_red )))) ;; Highlight for bracket without pair

   ;; Flycheck
   `(flycheck-warning                          ((t (:foreground ,darktooth-white :background ,darktooth-faded_red :box t :bold t :underline t ))))

   ;; elscreen
   `(elscreen-tab-background-face              ((t ( :background ,darktooth-dark0 :box nil )))) ;; Tab bar, not the tabs
   `(elscreen-tab-control-face                 ((t (:foreground ,darktooth-neutral_red :background ,darktooth-dark2 :box nil :underline nil )))) ;; The controls
   `(elscreen-tab-current-screen-face          ((t (:foreground ,darktooth-dark0 :background ,darktooth-dark4 :box nil )))) ;; Current tab
   `(elscreen-tab-other-screen-face            ((t (:foreground ,darktooth-light4 :background ,darktooth-dark2 :box nil :underline nil )))) ;; Inactive tab

   ;; ag (The Silver Searcher)
   `(ag-hit-face                               ((t (:foreground ,darktooth-neutral_blue ))))
   `(ag-match-face                             ((t (:foreground ,darktooth-neutral_red ))))

   ;; Diffs
   `(diff-changed                              ((t (:foreground ,darktooth-light1 :background nil ))))
   `(diff-added                                ((t (:foreground ,darktooth-neutral_green :background nil ))))
   `(diff-removed                              ((t (:foreground ,darktooth-neutral_red :background nil ))))
   `(diff-indicator-changed                    ((t (:inherit diff-changed ))))
   `(diff-indicator-added                      ((t (:inherit diff-added ))))
   `(diff-indicator-removed                    ((t (:inherit diff-removed ))))

   `(js2-warning                               ((t (:underline (:color ,darktooth-bright_yellow :style wave) ))))
   `(js2-error                                 ((t (:underline (:color ,darktooth-bright_red :style wave) ))))
   `(js2-external-variable                     ((t (:underline (:color ,darktooth-bright_aqua :style wave) ))))
   `(js2-jsdoc-tag                             ((t (:foreground ,darktooth-medium :background nil ))))
   `(js2-jsdoc-type                            ((t (:foreground ,darktooth-light4 :background nil ))))
   `(js2-jsdoc-value                           ((t (:foreground ,darktooth-light3 :background nil ))))
   `(js2-function-param                        ((t (:foreground ,darktooth-bright_aqua :background nil ))))
   `(js2-function-call                         ((t (:foreground ,darktooth-bright_blue :background nil ))))
   `(js2-instance-member                       ((t (:foreground ,darktooth-bright_orange :background nil ))))
   `(js2-private-member                        ((t (:foreground ,darktooth-faded_yellow :background nil ))))
   `(js2-private-function-call                 ((t (:foreground ,darktooth-faded_aqua :background nil ))))
   `(js2-jsdoc-html-tag-name                   ((t (:foreground ,darktooth-light4 :background nil ))))
   `(js2-jsdoc-html-tag-delimiter              ((t (:foreground ,darktooth-light3 :background nil ))))

   ;; Magit
   `(magit-blame-date                    ((t (:foreground ,darktooth-faded_red))))
   `(magit-blame-hash                    ((t (:foreground ,darktooth-faded_red))))
   `(magit-blame-heading                 ((t (:foreground ,darktooth-faded_red))))
   `(magit-blame-name                    ((t (:foreground ,darktooth-faded_red))))
   `(magit-blame-summary                 ((t (:foreground ,darktooth-faded_red))))
   `(magit-branch                        ((t (:foreground ,darktooth-aquamarine4))))
   `(magit-branch-remote                 ((t (:foreground ,darktooth-bright_green :weight bold))))
   `(magit-cherry-equivalent             ((t (:foreground ,darktooth-neutral_orange))))
   `(magit-cherry-unmatched              ((t (:foreground ,darktooth-neutral_purple))))
   `(magit-diff-add                      ((t (:background ,darktooth-bright_green))))
   `(magit-diff-added                    ((t (:foreground ,darktooth-neutral_aqua))))
   `(magit-diff-added-highlight          ((t (:foreground ,darktooth-neutral_aqua :weight bold :background ,darktooth-dark1))))
   `(magit-diff-base                     ((t (:foreground ,darktooth-dark0))))
   `(magit-diff-base-highlight           ((t (:foreground ,darktooth-dark0 :background ,darktooth-dark1))))
   `(magit-diff-conflict-heading         ((t (:foreground ,darktooth-faded_red))))
   `(magit-diff-context                  ((t (:foreground ,darktooth-dark3))))
   `(magit-diff-context-highlight        ((t (:foreground ,darktooth-dark3 :background ,darktooth-dark1))))
   `(magit-diff-del                      ((t (:foreground ,darktooth-bright_red))))
   `(magit-diff-file-header              ((t (:foreground ,darktooth-bright_blue))))
   `(magit-diff-file-heading             ((t (:foreground ,darktooth-light0_soft))))
   `(magit-diff-file-heading-highlight   ((t (:foreground ,darktooth-light0_soft :background ,darktooth-dark1 :weight bold))))
   `(magit-diff-file-heading-selection   ((t (:foreground ,darktooth-light0_soft))))
   `(magit-diff-hunk-heading             ((t (:foreground ,darktooth-light3 :background ,darktooth-dark2))))
   `(magit-diff-hunk-heading-highlight   ((t (:foreground ,darktooth-light2 :background ,darktooth-dark3))))
   `(magit-diff-hunk-heading-selection   ((t (:foreground ,darktooth-bright_aqua :weight bold))))
   `(magit-diff-lines-boundary           ((t (:foreground ,darktooth-faded_red))))
   `(magit-diff-lines-heading            ((t (:foreground ,darktooth-faded_red))))
   `(magit-diff-merge-current            ((t (:background ,darktooth-dark_yellow))))
   `(magit-diff-merge-diff3-separator    ((t (:foreground ,darktooth-neutral_orange :weight bold))))
   `(magit-diff-merge-proposed           ((t (:background ,darktooth-dark_green))))
   `(magit-diff-merge-separator          ((t (:foreground ,darktooth-neutral_orange))))
   `(magit-diff-none                     ((t (:foreground ,darktooth-medium))))
   `(magit-diff-our                      ((t (:foreground ,darktooth-faded_red))))
   `(magit-diff-our-highlight            ((t (:foreground ,darktooth-faded_red))))
   `(magit-diff-removed                  ((t (:foreground ,darktooth-neutral_red ))))
   `(magit-diff-removed-highlight        ((t (:foreground ,darktooth-neutral_red :weight bold :background ,darktooth-dark1))))
   `(magit-diff-their                    ((t (:foreground ,darktooth-faded_red))))
   `(magit-diff-their-highlight          ((t (:foreground ,darktooth-faded_red))))
   `(magit-diff-whitespace-warning       ((t (:foreground ,darktooth-faded_red))))
   `(magit-diffstat-added                ((t (:foreground ,darktooth-bright_green))))
   `(magit-diffstat-removed              ((t (:foreground ,darktooth-bright_red))))
   `(magit-item-highlight                ((t (:background ,darktooth-dark0_hard :weight normal))))
   `(magit-item-mark                     ((t (:background ,darktooth-dark0))))
   `(magit-key-mode-args-face            ((t (:foreground ,darktooth-light4))))
   `(magit-key-mode-button-face          ((t (:foreground ,darktooth-neutral_orange :weight bold))))
   `(magit-key-mode-header-face          ((t (:foreground ,darktooth-light4 :weight bold))))
   `(magit-key-mode-switch-face          ((t (:foreground ,darktooth-turquoise4 :weight bold))))
   `(magit-log-author                    ((t (:foreground ,darktooth-neutral_aqua))))
   `(magit-log-date                      ((t (:foreground ,darktooth-faded_orange))))
   `(magit-log-graph                     ((t (:foreground ,darktooth-light1))))
   `(magit-log-head-label-bisect-bad     ((t (:foreground ,darktooth-bright_red))))
   `(magit-log-head-label-bisect-good    ((t (:foreground ,darktooth-bright_green))))
   `(magit-log-head-label-bisect-skip    ((t (:foreground ,darktooth-neutral_yellow))))
   `(magit-log-head-label-default        ((t (:foreground ,darktooth-neutral_blue))))
   `(magit-log-head-label-head           ((t (:foreground ,darktooth-faded_yellow :weight bold))))
   `(magit-log-head-label-local          ((t (:foreground ,darktooth-neutral_green :weight bold))))
   `(magit-log-head-label-patches        ((t (:foreground ,darktooth-faded_orange))))
   `(magit-log-head-label-remote         ((t (:foreground ,darktooth-neutral_purple :weight bold))))
   `(magit-log-head-label-tags           ((t (:foreground ,darktooth-neutral_green))))
   `(magit-log-head-label-wip            ((t (:foreground ,darktooth-neutral_red))))
   `(magit-log-message                   ((t (:foreground ,darktooth-light1))))
   `(magit-log-reflog-label-amend        ((t (:foreground ,darktooth-bright_blue))))
   `(magit-log-reflog-label-checkout     ((t (:foreground ,darktooth-bright_yellow))))
   `(magit-log-reflog-label-cherry-pick  ((t (:foreground ,darktooth-neutral_red))))
   `(magit-log-reflog-label-commit       ((t (:foreground ,darktooth-neutral_green))))
   `(magit-log-reflog-label-merge        ((t (:foreground ,darktooth-bright_green))))
   `(magit-log-reflog-label-other        ((t (:foreground ,darktooth-faded_red))))
   `(magit-log-reflog-label-rebase       ((t (:foreground ,darktooth-bright_purple))))
   `(magit-log-reflog-label-remote       ((t (:foreground ,darktooth-neutral_orange))))
   `(magit-log-reflog-label-reset        ((t (:foreground ,darktooth-neutral_yellow))))
   `(magit-log-sha1                      ((t (:foreground ,darktooth-sienna))))
   `(magit-process-ng                    ((t (:foreground ,darktooth-bright_red :weight bold))))
   `(magit-process-ok                    ((t (:foreground ,darktooth-bright_green :weight bold))))
   `(magit-section-heading               ((t (:foreground ,darktooth-neutral_orange :weight bold))))
   `(magit-section-highlight             ((t (:background ,darktooth-dark1 :weight bold))))
   `(magit-signature-bad                 ((t (:foreground ,darktooth-bright_red :weight bold))))
   `(magit-signature-good                ((t (:foreground ,darktooth-bright_green :weight bold))))
   `(magit-signature-none                ((t (:foreground ,darktooth-faded_red))))
   `(magit-signature-untrusted           ((t (:foreground ,darktooth-bright_purple :weight bold))))
   `(magit-tag                           ((t (:foreground ,darktooth-neutral_green))))
   `(magit-whitespace-warning-face       ((t (:background ,darktooth-faded_red))))

   ;; org-mode
   `(org-agenda-date-today                     ((t (:foreground ,darktooth-light2 :slant italic :weight bold))) t)
   `(org-agenda-structure                      ((t (:inherit font-lock-comment-face ))))
   `(org-archived                              ((t (:foreground ,darktooth-light0 :weight bold ))))
   `(org-checkbox                              ((t (:foreground ,darktooth-light2 :background ,darktooth-dark0 :box ( :line-width 1 :style released-button) ))))
   `(org-date                                  ((t (:foreground ,darktooth-faded_blue :underline t ))))
   `(org-deadline-announce                     ((t (:foreground ,darktooth-faded_red ))))
   `(org-done                                  ((t (:foreground ,darktooth-bright_green :bold t :weight bold ))))
   `(org-formula                               ((t (:foreground ,darktooth-bright_yellow ))))
   `(org-headline-done                         ((t (:foreground ,darktooth-bright_green ))))
   `(org-hide                                  ((t (:foreground ,darktooth-dark0 ))))
   `(org-level-1                               ((t (:foreground ,darktooth-bright_orange ))))
   `(org-level-2                               ((t (:foreground ,darktooth-bright_green ))))
   `(org-level-3                               ((t (:foreground ,darktooth-bright_blue ))))
   `(org-level-4                               ((t (:foreground ,darktooth-bright_yellow ))))
   `(org-level-5                               ((t (:foreground ,darktooth-faded_aqua ))))
   `(org-level-6                               ((t (:foreground ,darktooth-bright_green ))))
   `(org-level-7                               ((t (:foreground ,darktooth-bright_red ))))
   `(org-level-8                               ((t (:foreground ,darktooth-bright_blue ))))
   `(org-link                                  ((t (:foreground ,darktooth-bright_yellow :underline t ))))
   `(org-scheduled                             ((t (:foreground ,darktooth-bright_green ))))
   `(org-scheduled-previously                  ((t (:foreground ,darktooth-bright_red ))))
   `(org-scheduled-today                       ((t (:foreground ,darktooth-bright_blue ))))
   `(org-sexp-date                             ((t (:foreground ,darktooth-bright_blue :underline t ))))
   `(org-special-keyword                       ((t (:inherit font-lock-comment-face ))))
   `(org-table                                 ((t (:foreground ,darktooth-bright_green ))))
   `(org-tag                                   ((t (:bold t :weight bold ))))
   `(org-time-grid                             ((t (:foreground ,darktooth-bright_orange ))))
   `(org-todo                                  ((t (:foreground ,darktooth-bright_red :weight bold :bold t ))))
   `(org-upcoming-deadline                     ((t (:inherit font-lock-keyword-face ))))
   `(org-warning                               ((t (:foreground ,darktooth-bright_red :weight bold :underline nil :bold t ))))
   `(org-column                                ((t ( :background ,darktooth-dark0 ))))
   `(org-column-title                          ((t ( :background ,darktooth-dark0_hard :underline t :weight bold ))))
   `(org-mode-line-clock                       ((t (:foreground ,darktooth-light2 :background ,darktooth-dark0 ))))
   `(org-mode-line-clock-overrun               ((t (:foreground ,darktooth-black :background ,darktooth-bright_red ))))
   `(org-ellipsis                              ((t (:foreground ,darktooth-bright_yellow :underline t ))))
   `(org-footnote                              ((t (:foreground ,darktooth-faded_aqua :underline t ))))

   ;; powerline
   `(powerline-active1                         ((t ( :background ,darktooth-dark2 :inherit mode-line ))))
   `(powerline-active2                         ((t ( :background ,darktooth-dark1 :inherit mode-line ))))
   `(powerline-inactive1                       ((t ( :background ,darktooth-medium :inherit mode-line-inactive ))))
   `(powerline-inactive2                       ((t ( :background ,darktooth-dark2 :inherit mode-line-inactive ))))

   ;; Matches and Isearch
   `(lazy-highlight                            ((t (:foreground ,darktooth-light0 :background ,darktooth-dark2 ))))
   `(highlight                                 ((t (:foreground ,darktooth-light0_hard :background ,darktooth-faded_blue))))
   `(match                                     ((t (:foreground ,darktooth-light0 :background ,darktooth-dark2 ))))
   `(isearch                                   ((t (:foreground ,darktooth-light0 :background ,darktooth-faded_aqua ))))
   `(isearch-fail                              ((t (:foreground ,darktooth-light0_hard :background ,darktooth-faded_red ))))
   `(show-paren-match                          ((t (:foreground ,darktooth-light0 :background ,darktooth-faded_blue ))))
   `(show-paren-mismatch                       ((t (:foreground ,darktooth-light0_hard :background ,darktooth-faded_red ))))
   `(anzu-mode-line                            ((t (:foreground ,darktooth-light0 :height 100 :background ,darktooth-faded_blue ))))

   ;; Popup
   `(popup-face                                ((t (:foreground ,darktooth-light0 :background ,darktooth-dark1 ))))
   `(popup-menu-mouse-face                     ((t (:foreground ,darktooth-light0 :background ,darktooth-faded_blue ))))
   `(popup-menu-selection-face                 ((t (:foreground ,darktooth-light0 :background ,darktooth-faded_blue ))))
   `(popup-tip-face                            ((t (:foreground ,darktooth-light0_hard :background ,darktooth-dark_aqua ))))
   ;; Use tip colors for the pos-tip color vars (see below)

   ;; Company Mode
   `(company-echo                              ((t (:inherit company-echo-common ))))
   `(company-echo-common                       ((t (:foreground ,darktooth-bright_blue :background nil ))))
   `(company-preview-common                    ((t (:underline ,darktooth-light1 ))))
   `(company-preview                           ((t (:inherit company-preview-common ))))
   `(company-preview-search                    ((t (:inherit company-preview-common ))))
   `(company-template-field                    ((t (:foreground ,darktooth-bright_blue :background nil :underline ,darktooth-dark_blue ))))
   `(company-scrollbar-fg                      ((t (:foreground nil :background ,darktooth-dark2 ))))
   `(company-scrollbar-bg                      ((t (:foreground nil :background ,darktooth-dark3 ))))
   `(company-tooltip                           ((t (:foreground ,darktooth-light0_hard :background ,darktooth-dark1 ))))
   `(company-preview-common                    ((t (:inherit font-lock-comment-face ))))
   `(company-tooltip-common                    ((t (:foreground ,darktooth-light0 :background ,darktooth-dark1 ))))
   `(company-tooltip-annotation                ((t (:foreground ,darktooth-bright_blue :background ,darktooth-dark1 ))))
   `(company-tooltip-common-selection          ((t (:foreground ,darktooth-light0 :background ,darktooth-faded_blue ))))
   `(company-tooltip-mouse                     ((t (:foreground ,darktooth-dark0 :background ,darktooth-bright_blue ))))
   `(company-tooltip-selection                 ((t (:foreground ,darktooth-light0 :background ,darktooth-faded_blue ))))

   ;; helm
   `(helm-M-x-key                              ((t (:foreground ,darktooth-neutral_orange ))))
   `(helm-action                               ((t (:foreground ,darktooth-white :underline t ))))
   `(helm-bookmark-addressbook                 ((t (:foreground ,darktooth-neutral_red ))))
   `(helm-bookmark-directory                   ((t (:foreground ,darktooth-bright_purple ))))
   `(helm-bookmark-file                        ((t (:foreground ,darktooth-faded_blue ))))
   `(helm-bookmark-gnus                        ((t (:foreground ,darktooth-faded_purple ))))
   `(helm-bookmark-info                        ((t (:foreground ,darktooth-turquoise4 ))))
   `(helm-bookmark-man                         ((t (:foreground ,darktooth-sienna ))))
   `(helm-bookmark-w3m                         ((t (:foreground ,darktooth-neutral_yellow ))))
   `(helm-buffer-directory                     ((t (:foreground ,darktooth-white :background ,darktooth-bright_blue ))))
   `(helm-buffer-not-saved                     ((t (:foreground ,darktooth-faded_red ))))
   `(helm-buffer-process                       ((t (:foreground ,darktooth-burlywood4 ))))
   `(helm-buffer-saved-out                     ((t (:foreground ,darktooth-bright_red ))))
   `(helm-buffer-size                          ((t (:foreground ,darktooth-bright_purple ))))
   `(helm-candidate-number                     ((t (:foreground ,darktooth-neutral_green ))))
   `(helm-ff-directory                         ((t (:foreground ,darktooth-neutral_purple ))))
   `(helm-ff-executable                        ((t (:foreground ,darktooth-turquoise4 ))))
   `(helm-ff-file                              ((t (:foreground ,darktooth-sienna ))))
   `(helm-ff-invalid-symlink                   ((t (:foreground ,darktooth-white :background ,darktooth-bright_red ))))
   `(helm-ff-prefix                            ((t (:foreground ,darktooth-black :background ,darktooth-neutral_yellow ))))
   `(helm-ff-symlink                           ((t (:foreground ,darktooth-neutral_orange ))))
   `(helm-grep-cmd-line                        ((t (:foreground ,darktooth-neutral_green ))))
   `(helm-grep-file                            ((t (:foreground ,darktooth-faded_purple ))))
   `(helm-grep-finish                          ((t (:foreground ,darktooth-turquoise4 ))))
   `(helm-grep-lineno                          ((t (:foreground ,darktooth-neutral_orange ))))
   `(helm-grep-match                           ((t (:foreground ,darktooth-neutral_yellow ))))
   `(helm-grep-running                         ((t (:foreground ,darktooth-neutral_red ))))
   `(helm-header                               ((t (:foreground ,darktooth-aquamarine4 ))))
   `(helm-helper                               ((t (:foreground ,darktooth-aquamarine4 ))))
   `(helm-history-deleted                      ((t (:foreground ,darktooth-black :background ,darktooth-bright_red ))))
   `(helm-history-remote                       ((t (:foreground ,darktooth-faded_red ))))
   `(helm-lisp-completion-info                 ((t (:foreground ,darktooth-faded_orange ))))
   `(helm-lisp-show-completion                 ((t (:foreground ,darktooth-bright_red ))))
   `(helm-locate-finish                        ((t (:foreground ,darktooth-white :background ,darktooth-aquamarine4 ))))
   `(helm-match                                ((t (:foreground ,darktooth-neutral_orange ))))
   `(helm-moccur-buffer                        ((t (:foreground ,darktooth-bright_aqua :underline t ))))
   `(helm-prefarg                              ((t (:foreground ,darktooth-turquoise4 ))))
   `(helm-selection                            ((t (:foreground ,darktooth-white :background ,darktooth-dark2 ))))
   `(helm-selection-line                       ((t (:foreground ,darktooth-white :background ,darktooth-dark2 ))))
   `(helm-separator                            ((t (:foreground ,darktooth-faded_red ))))
   `(helm-source-header                        ((t (:foreground ,darktooth-light2 :background ,darktooth-black :bold t))))
   `(helm-visible-mark                         ((t (:foreground ,darktooth-black :background ,darktooth-light3 ))))

   ;; Term
   `(term-color-black                          ((t (:foreground ,darktooth-dark1 ))))
   `(term-color-blue                           ((t (:foreground ,darktooth-neutral_blue ))))
   `(term-color-cyan                           ((t (:foreground ,darktooth-neutral_aqua ))))
   `(term-color-green                          ((t (:foreground ,darktooth-neutral_green ))))
   `(term-color-magenta                        ((t (:foreground ,darktooth-neutral_purple ))))
   `(term-color-red                            ((t (:foreground ,darktooth-neutral_red ))))
   `(term-color-white                          ((t (:foreground ,darktooth-light1 ))))
   `(term-color-yellow                         ((t (:foreground ,darktooth-neutral_yellow ))))
   `(term-default-fg-color                     ((t (:foreground ,darktooth-light0 ))))
   `(term-default-bg-color                     ((t (:background ,darktooth-dark0 )))))

  (custom-theme-set-variables
   'darktooth

   `(pos-tip-foreground-color ,darktooth-light0_hard)
   `(pos-tip-background-color ,darktooth-dark_aqua)

   `(ansi-color-names-vector [,darktooth-dark1
                              ,darktooth-neutral_red
                              ,darktooth-neutral_green
                              ,darktooth-neutral_yellow
                              ,darktooth-neutral_blue
                              ,darktooth-neutral_purple
                              ,darktooth-neutral_aqua
                              ,darktooth-light1])))
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))


(provide-theme 'darktooth-custom)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; darktooth-theme.el ends here
