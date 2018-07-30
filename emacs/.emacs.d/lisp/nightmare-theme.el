;;; nightmare-theme.el --- Purple remix of darktooth

;; Copyright (c) 2017 Lowe Thiderman (GNU/GPL Licence)

;; Authors: Lowe Thiderman <lowe.thiderman@gmail.com>
;; URL: http://github.com/thiderman/dotfiles
;; Version: 0.3.7
;; Package-Requires: ((autothemer "0.2"))

;;; Commentary:
;;  A purple remix of darktooth
;;
;;  This is a remix of the excellent darktooth theme which makes it
;;  100x more purple. It currently only supports GUI colors.
;;  Credits to jasonm23 for making the original!

;;; Code:
(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(autothemer-deftheme
 nightmare "A purple remix of darktooth"

 ((((class color) (min-colors #xFFFFFF)) ;; color column 1 GUI/24bit
   ((class color) (min-colors #xFF)))    ;; color column 2 Xterm/256

  (nightmare-dark0_hard      "#2D2031" "#1c1c1c")
  (nightmare-dark0           "#281a28" "#262626")
  (nightmare-dark0_soft      "#32243F" "#303030")
  (nightmare-dark1           "#3C3246" "#3a3a3a")
  (nightmare-dark2           "#503065" "#4e4e4e")
  (nightmare-dark3           "#665064" "#626262")
  (nightmare-dark4           "#5C3074" "#767676")

  (nightmare-medium          "#927F92" "#8a8a8a")

  (nightmare-light0_hard     "#FFFFC8" "#ffffdf")
  (nightmare-light0          "#FFDDD4" "#ffffaf")
  (nightmare-light0_soft     "#F8D1CD" "#ffff87")
  (nightmare-light1          "#EFC4C5" "#ffdfaf")
  (nightmare-light2          "#D9ADB4" "#bcbcbc")
  (nightmare-light3          "#C197A6" "#a8a8a8")
  (nightmare-light4          "#AC8297" "#949494")

  (nightmare-bright_red      "#FB2133" "#d75f5f")
  (nightmare-bright_green    "#88f326" "#afaf00")
  (nightmare-bright_yellow   "#FA952F" "#ffaf00")
  (nightmare-bright_blue     "#837D98" "#87afaf")
  (nightmare-bright_purple   "#D350DA" "#d787af")
  (nightmare-bright_aqua     "#8EC07C" "#87af87")
  (nightmare-bright_orange   "#d237af" "#ff8700")
  (nightmare-bright_cyan     "#3FD7E5" "#00d7ff")

  ;; neutral, no 256-color code, requested, nice work-around meanwhile
  (nightmare-neutral_red     "#FB2020" "#D75F5F")
  (nightmare-neutral_green   "#B8BB26" "#73AF00")
  (nightmare-neutral_yellow  "#FABD2F" "#FFAF00")
  (nightmare-neutral_blue    "#83A598" "#87AFAF")
  (nightmare-neutral_purple  "#D3869B" "#D787AF")
  (nightmare-neutral_aqua    "#8EC07C" "#87AF87")
  (nightmare-neutral_orange  "#FE8019" "#FF8700")
  (nightmare-neutral_cyan    "#17CCD5" "#17CCD5")

  (nightmare-faded_red       "#9D0006" "#870000")
  (nightmare-faded_green     "#79740E" "#878700")
  (nightmare-faded_yellow    "#B57614" "#af8700")
  (nightmare-faded_blue      "#674678" "#005f87")
  (nightmare-faded_purple    "#8F3F71" "#875f87")
  (nightmare-faded_aqua      "#427B58" "#5f8787")
  (nightmare-faded_orange    "#AF3A03" "#af5f00")

  (nightmare-faded_cyan      "#00A7AF" "#00afaf")

  (nightmare-muted_red       "#901A1E" "#870000")
  (nightmare-muted_green     "#556C21" "#878700")
  (nightmare-muted_yellow    "#A87933" "#af8700")
  (nightmare-muted_blue      "#1B5C6B" "#005f87")
  (nightmare-muted_purple    "#82526E" "#875f87")
  (nightmare-muted_aqua      "#506E59" "#5f8787")
  (nightmare-muted_orange    "#A24921" "#af5f00")
  (nightmare-muted_cyan      "#18A7AF" "#00afaf")

  (nightmare-dark_red        "#421E1E" "#5f0000")
  (nightmare-dark_green      "#232B0F" "#005f00")
  (nightmare-dark_yellow     "#4D3B27" "#5f5f00")
  (nightmare-dark_blue       "#2B3C44" "#00005f")
  (nightmare-dark_purple     "#6E2D65" "#5f00af")
  (nightmare-dark_aqua       "#36473A" "#005f5f")
  (nightmare-dark_orange     "#613620" "#af5f00")
  (nightmare-dark_cyan       "#205161" "#005f87")

  (nightmare-mid_red         "#3F1B1B" "#5f0000")
  (nightmare-mid_green       "#1F321C" "#005f00")
  (nightmare-mid_yellow      "#4C3A25" "#5f5f00")
  (nightmare-mid_blue        "#30434C" "#00005f")
  (nightmare-mid_purple      "#4C3B43" "#5f00af")
  (nightmare-mid_aqua        "#394C3D" "#005f5f")
  (nightmare-mid_orange      "#603000" "#af5f00")
  (nightmare-mid_cyan        "#005560" "#005f87")

  (nightmare-delimiter-one   "#5C5F81" "#5f8787")
  (nightmare-delimiter-two   "#835586" "#875f5f")
  (nightmare-delimiter-three "#9C5068" "#af5f5f")
  (nightmare-delimiter-four  "#7B475C" "#5f5f5f")

  ;; 24 bit has tints from light0 and terminal cycles through
  ;; the 4 nightmare-delimiter colors
  (nightmare-identifiers-1   "#E17FC6" "#ffdfaf")
  (nightmare-identifiers-2   "#E090C6" "#dfdfaf")
  (nightmare-identifiers-3   "#D690C6" "#dfe5c5")
  (nightmare-identifiers-4   "#CB90C6" "#ffd7af")
  (nightmare-identifiers-5   "#C690CB" "#dfdf87")
  (nightmare-identifiers-6   "#C690D6" "#dfdfdf")
  (nightmare-identifiers-7   "#C690E0" "#afdfdf")
  (nightmare-identifiers-8   "#C68AE6" "#dfdfff")
  (nightmare-identifiers-9   "#C680E6" "#afdfff")
  (nightmare-identifiers-10  "#C675E6" "#dfafff")
  (nightmare-identifiers-11  "#CB70E6" "#afafff")
  (nightmare-identifiers-12  "#D670E6" "#dfafaf")
  (nightmare-identifiers-13  "#E070E6" "#dfc5e5")
  (nightmare-identifiers-14  "#E670E0" "#ffafaf")
  (nightmare-identifiers-15  "#E670D6" "#dfdfff")

  (nightmare-white           "#FFFFFF" "white")
  (nightmare-black           "#000000" "black")
  (nightmare-sienna          "#FE20a9" "sienna")
  (nightmare-darkslategray4  "HotPink4" "DarkSlateGray4")
  (nightmare-lightblue4      "#5659fD" "LightBlue4")
  (nightmare-burlywood4      "VioletRed2" "burlywood4")
  (nightmare-aquamarine4     "#8365b8" "aquamarine4")
  (nightmare-turquoise4      "#61ACBB" "turquoise4"))

 ((default                                   (:foreground nightmare-light0 :background nightmare-dark0))
  (cursor                                    (:background nightmare-light0))
  (link                                      (:foreground nightmare-bright_blue :underline t))
  (link-visited                              (:foreground nightmare-bright_blue :underline nil))


  (fringe                                    (:background nightmare-dark0_hard :foreground nightmare-dark2))
  (vertical-border                           (:background nightmare-dark0 :foreground nightmare-dark2))
  (linum                                     (:foreground nightmare-dark4))
  (hl-line                                   (:background nightmare-dark_purple))
  (region                                    (:background nightmare-dark2 :distant-foreground nightmare-light0))
  (secondary-selection                       (:background nightmare-dark_blue))
  (cua-rectangle                             (:background nightmare-mid_blue))
  (header-line                               (:foreground nightmare-turquoise4 :background nightmare-dark0 :bold nil))
  (minibuffer-prompt                         (:foreground nightmare-bright_cyan :background nightmare-dark0 :bold nil))

  ;; compilation messages (also used by several other modes)
  (compilation-info                          (:foreground nightmare-neutral_green))
  (compilation-mode-line-fail                (:foreground nightmare-neutral_red))
  (error                                     (:foreground nightmare-bright_orange :bold t))
  (success                                   (:foreground nightmare-neutral_green :bold t))
  (warning                                   (:foreground nightmare-bright_red :bold t))

  ;; Built-in syntax
  (font-lock-builtin-face                            (:bold t :foreground nightmare-bright_orange))
  (font-lock-constant-face                           (:bold t :foreground nightmare-burlywood4))
  (font-lock-comment-face                            (:bold t :foreground nightmare-muted_purple))
  (font-lock-function-name-face                      (:bold t :foreground nightmare-bright_purple))
  (font-lock-keyword-face                            (:bold t :foreground nightmare-sienna))
  (font-lock-string-face                             (:bold t :foreground nightmare-neutral_purple))
  (font-lock-variable-name-face                      (:bold t :foreground nightmare-aquamarine4))
  (font-lock-type-face                               (:bold t :foreground nightmare-bright_aqua))
  (font-lock-warning-face                            (:bold t :foreground nightmare-neutral_red))
  (font-lock-fixme-face                              (:bold t :foreground nightmare-neutral_red))

  ;; MODE SUPPORT: eldoc
  (eldoc-highlight-function-argument         (:inherit 'font-lock-keyword-face))

  ;; MODE SUPPORT: which-key
  (which-key-key-face                        (:inherit 'font-lock-variable-name-face))

  ;; MODE SUPPORT: which-func
  (which-func                                (:inherit 'font-lock-function-name-face))

  ;; MODE SUPPORT: elixir-mode
  (elixir-atom-face                          (:foreground nightmare-lightblue4))
  (elixir-attribute-face                     (:foreground nightmare-burlywood4))

  ;; MODE SUPPORT: man
  (Man-overstrike                            (:foreground nightmare-bright_red :bold t))
  (Man-underline                             (:foreground nightmare-bright_green :bold t))

  ;; MODE SUPPORT: woman
  (woman-bold                               (:foreground nightmare-bright_red :bold t))
  (woman-italic                             (:foreground nightmare-bright_green :bold t))

  ;; MODE SUPPORT: tldr
  (tldr-command-itself                       (:foreground nightmare-bright_red :bold t))
  (tldr-command-argument                     (:foreground nightmare-bright_green :bold t))
  (tldr-code-block                           (:foreground nightmare-bright_blue :bold t))
  (tldr-description                          (:inherit 'default))
  (tldr-title                                (:foreground nightmare-bright_red :bold t :height 1.2))
  (tldr-introduction                         (:inherit 'font-lock-comment-face :slant 'italic))

  ;; MODE SUPPORT: markdown
  (markdown-header-face                      (:inherit font-lock-function-name-face :bold t :height 3.0 :family "Yanone Kaffeesatz"))
  (markdown-header-face-2                    (:inherit font-lock-builtin-face :bold t :height 2.0 :family "Yanone Kaffeesatz"))
  (markdown-header-face-3                    (:inherit font-lock-string-face :bold t :height 2.0 :family "Yanone Kaffeesatz"))
  (markdown-header-face-4                    (:inherit font-lock-variable-name-face :bold t :height 2.0 :family "Yanone Kaffeesatz"))
  (markdown-header-delimiter-face            (:inherit 'markdown-markup-face :family "Yanone Kaffeesatz" :height 2.0))
  (markdown-italic-face                      (:inherit 'italic :foreground nightmare-bright_blue))
  (markdown-list-face                        (:foreground nightmare-darkslategray4 :bold t))

  ;; MODE SUPPORT: whitespace-mode
  (whitespace-space                          (:foreground nightmare-dark4 :background nightmare-dark0))
  (whitespace-hspace                         (:foreground nightmare-dark4 :background nightmare-dark0))
  (whitespace-tab                            (:foreground nightmare-dark4 :background nightmare-dark0))
  (whitespace-newline                        (:foreground nightmare-dark4 :background nightmare-dark0))
  (whitespace-trailing                       (:foreground nightmare-neutral_red :background nightmare-dark1))
  (whitespace-line                           (:foreground nightmare-neutral_red :background nightmare-dark1))
  (whitespace-space-before-tab               (:foreground nightmare-dark4 :background nightmare-dark0))
  (whitespace-indentation                    (:foreground nightmare-dark4 :background nightmare-dark0))
  (whitespace-empty                          (:foreground nil :background nil))
  (whitespace-space-after-tab                (:foreground nightmare-dark4 :background nightmare-dark0))

  ;; MODE SUPPORT: rainbow-delimiters
  (rainbow-delimiters-depth-1-face           (:foreground nightmare-delimiter-one :bold t))
  (rainbow-delimiters-depth-2-face           (:foreground nightmare-delimiter-two :bold t))
  (rainbow-delimiters-depth-3-face           (:foreground nightmare-delimiter-three :bold t))
  (rainbow-delimiters-depth-4-face           (:foreground nightmare-delimiter-four :bold t))
  (rainbow-delimiters-depth-5-face           (:foreground nightmare-delimiter-one :bold t))
  (rainbow-delimiters-depth-6-face           (:foreground nightmare-delimiter-two :bold t))
  (rainbow-delimiters-depth-7-face           (:foreground nightmare-delimiter-three :bold t))
  (rainbow-delimiters-depth-8-face           (:foreground nightmare-delimiter-four :bold t))
  (rainbow-delimiters-depth-9-face           (:foreground nightmare-delimiter-one :bold t))
  (rainbow-delimiters-depth-10-face          (:foreground nightmare-delimiter-two :bold t))
  (rainbow-delimiters-depth-11-face          (:foreground nightmare-delimiter-three :bold t))
  (rainbow-delimiters-depth-12-face          (:foreground nightmare-delimiter-four :bold t))
  (rainbow-delimiters-unmatched-face         (:foreground nightmare-light0 :background nil :bold t))

  ;; MODE SUPPORT: rainbow-identifiers
  (rainbow-identifiers-identifier-1          (:foreground nightmare-identifiers-1 :bold t))
  (rainbow-identifiers-identifier-2          (:foreground nightmare-identifiers-2 :bold t))
  (rainbow-identifiers-identifier-3          (:foreground nightmare-identifiers-3 :bold t))
  (rainbow-identifiers-identifier-4          (:foreground nightmare-identifiers-4 :bold t))
  (rainbow-identifiers-identifier-5          (:foreground nightmare-identifiers-5 :bold t))
  (rainbow-identifiers-identifier-6          (:foreground nightmare-identifiers-6 :bold t))
  (rainbow-identifiers-identifier-7          (:foreground nightmare-identifiers-7 :bold t))
  (rainbow-identifiers-identifier-8          (:foreground nightmare-identifiers-8 :bold t))
  (rainbow-identifiers-identifier-9          (:foreground nightmare-identifiers-9 :bold t))
  (rainbow-identifiers-identifier-10         (:foreground nightmare-identifiers-10 :bold t))
  (rainbow-identifiers-identifier-11         (:foreground nightmare-identifiers-11 :bold t))
  (rainbow-identifiers-identifier-12         (:foreground nightmare-identifiers-12 :bold t))
  (rainbow-identifiers-identifier-13         (:foreground nightmare-identifiers-13 :bold t))
  (rainbow-identifiers-identifier-14         (:foreground nightmare-identifiers-14 :bold t))
  (rainbow-identifiers-identifier-15         (:foreground nightmare-identifiers-15 :bold t))

  ;; MODE SUPPORT: ido
  (ido-indicator                             (:background nightmare-bright_red :foreground nightmare-bright_yellow))
  (ido-subdir                                (:foreground nightmare-light3))
  (ido-first-match                           (:foreground nightmare-faded_cyan :background nightmare-dark0_hard))
  (ido-only-match                            (:foreground nightmare-darkslategray4))
  (ido-vertical-match-face                   (:bold t))
  (ido-vertical-only-match-face              (:foreground nightmare-bright_cyan))
  (ido-vertical-first-match-face             (:foreground nightmare-bright_cyan :background nightmare-dark_blue))

  ;; MODE SUPPORT: linum-relative
  (linum-relative-current-face               (:foreground nightmare-light4 :background nightmare-dark1))

  ;; MODE SUPPORT: highlight-indentation-mode
  (highlight-indentation-current-column-face (:background nightmare-dark4))
  (highlight-indentation-face                (:background nightmare-dark1))

  ;; MODE SUPPORT: highlight-numbers
  (highlight-numbers-number                  (:foreground nightmare-bright_purple :bold nil))

  ;; MODE SUPPORT: highlight-symbol
  (highlight-symbol-face                     (:foreground nightmare-neutral_purple))

  ;; MODE SUPPORT: hi-lock
  (hi-blue                                   (:foreground nightmare-dark0_hard :background nightmare-bright_blue ))
  (hi-green                                  (:foreground nightmare-dark0_hard :background nightmare-bright_green ))
  (hi-pink                                   (:foreground nightmare-dark0_hard :background nightmare-bright_purple ))
  (hi-yellow                                 (:foreground nightmare-dark0_hard :background nightmare-bright_yellow ))
  (hi-blue-b                                 (:foreground nightmare-bright_blue :bold t ))
  (hi-green-b                                (:foreground nightmare-bright_green :bold t ))
  (hi-red-b                                  (:foreground nightmare-bright_red :bold t  ))
  (hi-black-b                                (:foreground nightmare-bright_orange :background nightmare-dark0_hard :bold t  ))
  (hi-black-hb                               (:foreground nightmare-bright_cyan :background nightmare-dark0_hard :bold t  ))

  ;; MODE SUPPORT: smartparens
  (sp-pair-overlay-face                      (:background nightmare-dark2))
  (sp-show-pair-match-face                   (:background nightmare-dark2))
  (sp-show-pair-mismatch-face                (:background nightmare-neutral_red))

  ;; MODE SUPPORT: auctex
  (font-latex-math-face                      (:foreground nightmare-lightblue4))
  (font-latex-sectioning-5-face              (:foreground nightmare-neutral_green))
  (font-latex-string-face                    (:inherit 'font-lock-string-face))
  (font-latex-warning-face                   (:inherit 'warning))

  ;; MODE SUPPORT: elscreen
  (elscreen-tab-background-face              (:background nightmare-dark0 :box nil))
  (elscreen-tab-control-face                 (:foreground nightmare-neutral_red :background nightmare-dark2 :box nil :underline nil))
  (elscreen-tab-current-screen-face          (:foreground nightmare-dark0 :background nightmare-dark4 :box nil))
  (elscreen-tab-other-screen-face            (:foreground nightmare-light4 :background nightmare-dark2 :box nil :underline nil))

  ;; MODE SUPPORT: embrace
  (embrace-help-pair-face                    (:foreground nightmare-bright_blue))
  (embrace-help-separator-face               (:foreground nightmare-bright_orange))
  (embrace-help-key-face                     (:weight 'bold nightmare-bright_green))
  (embrace-help-mark-func-face               (:foreground nightmare-bright_cyan))

  ;; MODE SUPPORT: ag (The Silver Searcher)
  (ag-hit-face                               (:foreground nightmare-neutral_blue))
  (ag-match-face                             (:foreground nightmare-neutral_red))

  ;; MODE SUPPORT: RipGrep
  (ripgrep-hit-face                          (:inherit 'ag-hit-face))
  (ripgrep-match-face                        (:inherit 'ag-match-face))

  ;; MODE SUPPORT: diff
  (diff-changed                              (:foreground nightmare-light1 :background nil))
  (diff-added                                (:foreground nightmare-neutral_green :background nil))
  (diff-removed                              (:foreground nightmare-neutral_red :background nil))

  ;; MODE SUPPORT: diff-indicator
  (diff-indicator-changed                    (:inherit 'diff-changed))
  (diff-indicator-added                      (:inherit 'diff-added))
  (diff-indicator-removed                    (:inherit 'diff-removed))

  ;; MODE SUPPORT: diff-hl
  (diff-hl-change                            (:inherit 'diff-changed))
  (diff-hl-delete                            (:inherit 'diff-removed))
  (diff-hl-insert                            (:inherit 'diff-added))

  (js2-warning                               (:underline (:color nightmare-bright_yellow :style 'wave)))
  (js2-error                                 (:underline (:color nightmare-bright_red :style 'wave)))
  (js2-external-variable                     (:underline (:color nightmare-bright_aqua :style 'wave)))
  (js2-jsdoc-tag                             (:foreground nightmare-medium :background nil))
  (js2-jsdoc-type                            (:foreground nightmare-light4 :background nil))
  (js2-jsdoc-value                           (:foreground nightmare-light3 :background nil))
  (js2-function-param                        (:foreground nightmare-bright_aqua :background nil))
  (js2-function-call                         (:foreground nightmare-bright_blue :background nil))
  (js2-instance-member                       (:foreground nightmare-bright_orange :background nil))
  (js2-private-member                        (:foreground nightmare-faded_yellow :background nil))
  (js2-private-function-call                 (:foreground nightmare-faded_aqua :background nil))
  (js2-jsdoc-html-tag-name                   (:foreground nightmare-light4 :background nil))
  (js2-jsdoc-html-tag-delimiter              (:foreground nightmare-light3 :background nil))

  ;; MODE SUPPORT: haskell
  (haskell-interactive-face-compile-warning  (:underline (:color nightmare-bright_yellow :style 'wave)))
  (haskell-interactive-face-compile-error    (:underline (:color nightmare-bright_red :style 'wave)))
  (haskell-interactive-face-garbage          (:foreground nightmare-dark4 :background nil))
  (haskell-interactive-face-prompt           (:foreground nightmare-light0 :background nil))
  (haskell-interactive-face-result           (:foreground nightmare-light3 :background nil))
  (haskell-literate-comment-face             (:foreground nightmare-light0 :background nil))
  (haskell-pragma-face                       (:foreground nightmare-medium :background nil))
  (haskell-constructor-face                  (:foreground nightmare-neutral_aqua :background nil))

  ;; MODE SUPPORT: org-mode
  (org-agenda-date-today                     (:foreground nightmare-muted_cyan :weight 'bold))
  (org-agenda-structure                      (:inherit 'font-lock-comment-face))
  (org-archived                              (:foreground nightmare-light0 :weight 'bold))
  (org-checkbox                              (:foreground nightmare-light2 :background nightmare-dark0 :box (:line-width 1 :style 'released-button)))
  (org-date                                  (:foreground nightmare-neutral_aqua :underline t))
  (org-deadline-announce                     (:foreground nightmare-faded_red :bold t))
  (org-document-info-keyword                 (:foreground nightmare-light2))
  (org-document-info                         (:foreground nightmare-identifiers-7))
  (org-document-title                        (:foreground nightmare-bright_cyan :weight 'bold))
  (org-done                                  (:foreground nightmare-bright_green :bold t :weight 'bold))
  (org-formula                               (:foreground nightmare-bright_yellow))
  (org-headline-done                         (:foreground nightmare-bright_green))
  (org-hide                                  (:foreground nightmare-dark0))
  (org-level-1                               (:foreground nightmare-neutral_green :bold t))
  (org-level-2                               (:foreground nightmare-bright_yellow :bold t))
  (org-level-3                               (:foreground nightmare-bright_blue :bold t))
  (org-level-4                               (:foreground nightmare-bright_aqua :bold t))
  (org-level-5                               (:foreground nightmare-neutral_purple :bold t))
  (org-level-6                               (:foreground nightmare-bright_green :bold t))
  (org-level-7                               (:foreground nightmare-bright_red :bold t))
  (org-level-8                               (:foreground nightmare-bright_blue :bold t))
  (org-link                                  (:foreground nightmare-bright_yellow :underline t :bold t))
  (org-scheduled                             (:foreground nightmare-bright_green))
  (org-scheduled-previously                  (:foreground nightmare-bright_red))
  (org-scheduled-today                       (:foreground nightmare-bright_blue :bold t))
  (org-sexp-date                             (:foreground nightmare-bright_blue :underline t))
  (org-special-keyword                       (:inherit 'font-lock-comment-face))
  (org-table                                 (:foreground nightmare-bright_green))
  (org-tag                                   (:bold t :weight 'bold))
  (org-time-grid                             (:foreground nightmare-bright_orange))
  (org-todo                                  (:foreground nightmare-bright_purple :bold t))
  (org-upcoming-deadline                     (:inherit 'font-lock-keyword-face))
  (org-warning                               (:foreground nightmare-bright_red :weight 'bold :underline nil :bold t))
  (org-column                                (:background nightmare-dark0))
  (org-column-title                          (:background nightmare-dark0_hard :underline t :weight 'bold))
  (org-mode-line-clock                       (:foreground nightmare-neutral_aqua :background nil))
  (org-mode-line-clock-overrun               (:foreground nightmare-black :background nightmare-bright_red))
  (org-ellipsis                              (:foreground nightmare-bright_yellow :underline t))
  (org-footnote                              (:foreground nightmare-faded_aqua :underline t))

  ;; MODE SUPPORT: powerline
  (powerline-active1                         (:background nightmare-dark2 :inherit 'mode-line))
  (powerline-active2                         (:background nightmare-dark1 :inherit 'mode-line))
  (powerline-inactive1                       (:background nightmare-dark0_soft :inherit 'mode-line-inactive))
  (powerline-inactive2                       (:background nightmare-dark2 :inherit 'mode-line-inactive))

  ;; MODE SUPPORT: smart-mode-line
  (sml/modes                                 (:foreground nightmare-light0_hard :weight 'bold :bold t))
  (sml/minor-modes                           (:foreground nightmare-neutral_orange))
  (sml/filename                              (:foreground nightmare-light0_hard :weight 'bold :bold t))
  (sml/prefix                                (:foreground nightmare-neutral_blue))
  (sml/git                                   (:inherit 'sml/prefix))
  (sml/process                               (:inherit 'sml/prefix))
  (sml/sudo                                  (:foreground nightmare-dark_orange :weight 'bold))
  (sml/read-only                             (:foreground nightmare-neutral_blue))
  (sml/outside-modified                      (:foreground nightmare-neutral_blue))
  (sml/modified                              (:foreground nightmare-neutral_blue))
  (sml/vc                                    (:foreground nightmare-faded_green))
  (sml/vc-edited                             (:foreground nightmare-bright_green))
  (sml/charging                              (:foreground nightmare-faded_aqua))
  (sml/discharging                           (:foreground nightmare-faded_aqua :weight 'bold))
  (sml/col-number                            (:foreground nightmare-neutral_orange))
  (sml/position-percentage                   (:foreground nightmare-faded_aqua))

  ;; Matches and Isearch
  (lazy-highlight                            (:foreground nightmare-light0 :background nightmare-dark2))
  (highlight                                 (:foreground nightmare-light0_hard :background nightmare-faded_blue))
  (match                                     (:foreground nightmare-light0 :background nightmare-dark2))

  ;; MODE SUPPORT: isearch
  (isearch                                   (:foreground nightmare-light0 :background nightmare-faded_aqua))
  (isearch-fail                              (:foreground nightmare-light0_hard :background nightmare-faded_red))

  ;; MODE SUPPORT: show-paren
  (show-paren-match                          (:foreground nightmare-light0 :background nightmare-faded_blue))
  (show-paren-mismatch                       (:foreground nightmare-light0_hard :background nightmare-faded_red))

  ;; MODE SUPPORT: anzu
  (anzu-mode-line                            (:foreground nightmare-light0 :height 100 :background nightmare-faded_blue))
  (anzu-match-1                              (:foreground nightmare-dark0 :background nightmare-bright_green))
  (anzu-match-2                              (:foreground nightmare-dark0 :background nightmare-bright_yellow))
  (anzu-match-3                              (:foreground nightmare-dark0 :background nightmare-bright_cyan))
  (anzu-replace-highlight                    (:background nightmare-dark_aqua))
  (anzu-replace-to                           (:background nightmare-dark_cyan))

  ;; MODE SUPPORT: el-search
  (el-search-match                           (:background nightmare-dark_cyan))
  (el-search-other-match                     (:background nightmare-dark_blue))

  ;; MODE SUPPORT: avy
  (avy-lead-face-0                           (:foreground nightmare-bright_blue ))
  (avy-lead-face-1                           (:foreground nightmare-bright_aqua ))
  (avy-lead-face-2                           (:foreground nightmare-bright_purple ))
  (avy-lead-face                             (:foreground nightmare-bright_red ))
  (avy-background-face                       (:foreground nightmare-dark3 ))
  (avy-goto-char-timer-face                  (:inherit 'highlight ))

  ;; MODE SUPPORT: popup
  (popup-face                                (:foreground nightmare-light0 :background nightmare-dark1))
  (popup-menu-mouse-face                     (:foreground nightmare-light0 :background nightmare-faded_blue))
  (popup-menu-selection-face                 (:foreground nightmare-light0 :background nightmare-faded_blue))
  (popup-tip-face                            (:foreground nightmare-light0_hard :background nightmare-dark_aqua))
  ;; Use tip colors for the pos-tip color vars (see below)

  ;; Inherit ac-dabbrev from popup menu face
  ;; MODE SUPPORT: ac-dabbrev
  (ac-dabbrev-menu-face                      (:inherit 'popup-face))
  (ac-dabbrev-selection-face                 (:inherit 'popup-menu-selection-face))

  ;; MODE SUPPORT: sh mode
  (sh-heredoc                                (:foreground nightmare-neutral_purple :background nil))
  (sh-quoted-exec                            (:foreground nightmare-neutral_purple :background nil))

  ;; MODE SUPPORT: company
  (company-echo                              (:inherit 'company-echo-common))
  (company-echo-common                       (:foreground nightmare-bright_blue :background nil))
  (company-preview-common                    (:underline nightmare-light1))
  (company-preview                           (:inherit 'company-preview-common))
  (company-preview-search                    (:inherit 'company-preview-common))
  (company-template-field                    (:foreground nightmare-bright_blue :background nil :underline nightmare-dark_blue))
  (company-scrollbar-fg                      (:foreground nil :background nightmare-dark2))
  (company-scrollbar-bg                      (:foreground nil :background nightmare-dark3))
  (company-tooltip                           (:foreground nightmare-light0_hard :background nightmare-dark1))
  (company-preview-common                    (:inherit 'font-lock-comment-face))
  (company-tooltip-common                    (:foreground nightmare-light0 :background nightmare-dark1))
  (company-tooltip-annotation                (:foreground nightmare-bright_blue :background nightmare-dark1))
  (company-tooltip-common-selection          (:foreground nightmare-light0 :background nightmare-faded_blue))
  (company-tooltip-mouse                     (:foreground nightmare-dark0 :background nightmare-bright_blue))
  (company-tooltip-selection                 (:foreground nightmare-light0 :background nightmare-faded_blue))

  ;; MODE SUPPORT: dired+
  (diredp-file-name                          (:foreground nightmare-light2 ))
  (diredp-file-suffix                        (:foreground nightmare-light4 ))
  (diredp-compressed-file-suffix             (:foreground nightmare-faded_cyan ))
  (diredp-dir-name                           (:foreground nightmare-faded_cyan ))
  (diredp-dir-heading                        (:foreground nightmare-bright_cyan ))
  (diredp-symlink                            (:foreground nightmare-bright_orange ))
  (diredp-date-time                          (:foreground nightmare-light3 ))
  (diredp-number                             (:foreground nightmare-faded_cyan ))
  (diredp-no-priv                            (:foreground nightmare-dark4 ))
  (diredp-other-priv                         (:foreground nightmare-dark2 ))
  (diredp-rare-priv                          (:foreground nightmare-dark4 ))
  (diredp-ignored-file-name                  (:foreground nightmare-dark4 ))

  (diredp-dir-priv                           (:foreground nightmare-faded_cyan  :background nightmare-dark_blue))
  (diredp-exec-priv                          (:foreground nightmare-faded_cyan  :background nightmare-dark_blue))
  (diredp-link-priv                          (:foreground nightmare-faded_aqua  :background nightmare-dark_aqua))
  (diredp-read-priv                          (:foreground nightmare-bright_red  :background nightmare-dark_red))
  (diredp-write-priv                         (:foreground nightmare-bright_aqua :background nightmare-dark_aqua))

  ;; MODE SUPPORT: helm
  (helm-M-x-key                              (:foreground nightmare-neutral_orange))
  (helm-action                               (:foreground nightmare-white :underline t))
  (helm-bookmark-addressbook                 (:foreground nightmare-neutral_red))
  (helm-bookmark-directory                   (:foreground nightmare-bright_purple))
  (helm-bookmark-file                        (:foreground nightmare-faded_blue))
  (helm-bookmark-gnus                        (:foreground nightmare-faded_purple))
  (helm-bookmark-info                        (:foreground nightmare-turquoise4))
  (helm-bookmark-man                         (:foreground nightmare-sienna))
  (helm-bookmark-w3m                         (:foreground nightmare-neutral_yellow))
  (helm-buffer-directory                     (:foreground nightmare-white :background nightmare-bright_blue))
  (helm-buffer-not-saved                     (:foreground nightmare-faded_red))
  (helm-buffer-process                       (:foreground nightmare-burlywood4))
  (helm-buffer-saved-out                     (:foreground nightmare-bright_red))
  (helm-buffer-size                          (:foreground nightmare-bright_purple))
  (helm-candidate-number                     (:foreground nightmare-neutral_green))
  (helm-ff-directory                         (:foreground nightmare-neutral_purple))
  (helm-ff-executable                        (:foreground nightmare-turquoise4))
  (helm-ff-file                              (:foreground nightmare-sienna))
  (helm-ff-invalid-symlink                   (:foreground nightmare-white :background nightmare-bright_red))
  (helm-ff-prefix                            (:foreground nightmare-black :background nightmare-neutral_yellow))
  (helm-ff-symlink                           (:foreground nightmare-neutral_orange))
  (helm-grep-cmd-line                        (:foreground nightmare-neutral_green))
  (helm-grep-file                            (:foreground nightmare-faded_purple))
  (helm-grep-finish                          (:foreground nightmare-turquoise4))
  (helm-grep-lineno                          (:foreground nightmare-neutral_orange))
  (helm-grep-match                           (:foreground nightmare-neutral_yellow))
  (helm-grep-running                         (:foreground nightmare-neutral_red))
  (helm-header                               (:foreground nightmare-aquamarine4))
  (helm-helper                               (:foreground nightmare-aquamarine4))
  (helm-history-deleted                      (:foreground nightmare-black :background nightmare-bright_red))
  (helm-history-remote                       (:foreground nightmare-faded_red))
  (helm-lisp-completion-info                 (:foreground nightmare-faded_orange))
  (helm-lisp-show-completion                 (:foreground nightmare-bright_red))
  (helm-locate-finish                        (:foreground nightmare-white :background nightmare-aquamarine4))
  (helm-match                                (:foreground nightmare-neutral_orange))
  (helm-moccur-buffer                        (:foreground nightmare-bright_aqua :underline t))
  (helm-prefarg                              (:foreground nightmare-turquoise4))
  (helm-selection                            (:foreground nightmare-white :background nightmare-dark2))
  (helm-selection-line                       (:foreground nightmare-white :background nightmare-dark2))
  (helm-separator                            (:foreground nightmare-faded_red))
  (helm-source-header                        (:foreground nightmare-light2 :background nightmare-dark1))
  (helm-visible-mark                         (:foreground nightmare-black :background nightmare-light3))

  ;; MODE SUPPORT: column-marker
  (column-marker-1                           (:background nightmare-faded_blue))
  (column-marker-2                           (:background nightmare-faded_purple))
  (column-marker-3                           (:background nightmare-faded_cyan))

  ;; MODE SUPPORT: vline
  (vline                                     (:background nightmare-dark_aqua))
  (vline-visual                              (:background nightmare-dark_aqua))

  ;; MODE SUPPORT: col-highlight
  (col-highlight                             (:inherit 'vline))

  ;; MODE SUPPORT: column-enforce-mode
  (column-enforce-face                       (:foreground nightmare-dark4 :background nightmare-dark_red))

  ;; MODE SUPPORT: hydra
  (hydra-face-red                            (:foreground nightmare-bright_red))
  (hydra-face-blue                           (:foreground nightmare-bright_blue))
  (hydra-face-pink                           (:foreground nightmare-identifiers-15))
  (hydra-face-amaranth                       (:foreground nightmare-faded_purple))
  (hydra-face-teal                           (:foreground nightmare-faded_cyan))

  ;; MODE SUPPORT: ivy
  (ivy-current-match                         (:foreground nightmare-light0 :background nightmare-faded_blue))
  (ivy-minibuffer-match-face-1               (:background nightmare-dark1))
  (ivy-minibuffer-match-face-2               (:background nightmare-dark2))
  (ivy-minibuffer-match-face-3               (:background nightmare-faded_aqua))
  (ivy-minibuffer-match-face-4               (:background nightmare-faded_purple))
  (ivy-confirm-face                          (:foreground nightmare-bright_green))
  (ivy-match-required-face                   (:foreground nightmare-bright_red))
  (ivy-remote                                (:foreground nightmare-neutral_blue))

  ;; MODE SUPPORT: smerge
  ;; TODO: smerge-base smerge-refined-changed
  (smerge-mine                               (:background nightmare-mid_purple))
  (smerge-other                              (:background nightmare-mid_blue))
  (smerge-markers                            (:background nightmare-dark0_soft))
  (smerge-refined-added                      (:background nightmare-dark_green))
  (smerge-refined-removed                    (:background nightmare-dark_red))

  ;; MODE SUPPORT: git-gutter
  (git-gutter:added                         (:foreground nightmare-neutral_green :background nightmare-dark0_hard))
  (git-gutter:deleted                       (:foreground nightmare-neutral_red :background nightmare-dark0_hard))
  (git-gutter:modified                      (:foreground nightmare-neutral_purple :background nightmare-dark0_hard))
  (git-gutter:separator                     (:foreground nightmare-neutral_cyan :background nightmare-dark0_hard))
  (git-gutter:unchanged                     (:foreground nightmare-neutral_yellow :background nightmare-dark0_hard))

  ;; MODE SUPPORT: git-gutter-fr
  (git-gutter-fr:added                      (:inherit 'git-gutter:added))
  (git-gutter-fr:deleted                    (:inherit 'git-gutter:deleted))
  (git-gutter-fr:modified                   (:inherit 'git-gutter:modified))

  ;; MODE SUPPORT: git-gutter+
  (git-gutter+-commit-header-face            (:inherit 'font-lock-comment-face))
  (git-gutter+-added                         (:foreground nightmare-muted_green :background nil ))
  (git-gutter+-deleted                       (:foreground nightmare-muted_red :background nil ))
  (git-gutter+-modified                      (:foreground nightmare-muted_purple :background nil ))
  (git-gutter+-separator                     (:foreground nightmare-muted_cyan :background nil ))
  (git-gutter+-unchanged                     (:foreground nightmare-muted_yellow :background nil ))


  ;; MODE SUPPORT: git-gutter-fr+
  (git-gutter-fr+-added                      (:inherit 'git-gutter+-added))
  (git-gutter-fr+-deleted                    (:inherit 'git-gutter+-deleted))
  (git-gutter-fr+-modified                   (:inherit 'git-gutter+-modified))

  ;; MODE SUPPORT: magit
  (git-commit-summary                        (:foreground nightmare-aquamarine4 :bold t))
  (magit-section-highlight                   (:background nightmare-dark0_soft))
  (magit-branch                              (:foreground nightmare-turquoise4 :background nil))
  (magit-branch-local                        (:foreground nightmare-turquoise4 :background nil))
  (magit-branch-remote                       (:foreground nightmare-aquamarine4 :background nil))
  (magit-cherry-equivalent                   (:foreground nightmare-neutral_orange))
  (magit-cherry-unmatched                    (:foreground nightmare-neutral_purple))
  (magit-diff-context                        (:foreground nightmare-dark3 :background nil))
  (magit-diff-context-highlight              (:foreground nightmare-dark4 :background nightmare-dark0_soft))
  (magit-diff-added                          (:foreground nightmare-bright_green :background nightmare-mid_green))
  (magit-diff-added-highlight                (:foreground nightmare-bright_green :background nightmare-mid_green))
  (magit-diff-removed                        (:foreground nightmare-bright_red :background nightmare-mid_red))
  (magit-diff-removed-highlight              (:foreground nightmare-bright_red :background nightmare-mid_red))
  (magit-diff-add                            (:foreground nightmare-bright_green))
  (magit-diff-del                            (:foreground nightmare-bright_red))
  (magit-diff-file-header                    (:foreground nightmare-bright_blue))
  (magit-diff-hunk-header                    (:foreground nightmare-neutral_aqua))
  (magit-diff-merge-current                  (:background nightmare-dark_yellow))
  (magit-diff-merge-diff3-separator          (:foreground nightmare-neutral_orange :weight 'bold))
  (magit-diff-merge-proposed                 (:background nightmare-dark_green))
  (magit-diff-merge-separator                (:foreground nightmare-neutral_orange))
  (magit-diff-none                           (:foreground nightmare-medium))
  (magit-item-highlight                      (:background nightmare-dark1 :weight 'normal))
  (magit-item-mark                           (:background nightmare-dark0))
  (magit-key-mode-args-face                  (:foreground nightmare-light4))
  (magit-key-mode-button-face                (:foreground nightmare-neutral_orange :weight 'bold))
  (magit-key-mode-header-face                (:foreground nightmare-light4 :weight 'bold))
  (magit-key-mode-switch-face                (:foreground nightmare-turquoise4 :weight 'bold))
  (magit-log-author                          (:foreground nightmare-neutral_aqua))
  (magit-log-date                            (:foreground nightmare-faded_orange))
  (magit-log-graph                           (:foreground nightmare-light1))
  (magit-log-head-label-bisect-bad           (:foreground nightmare-bright_red))
  (magit-log-head-label-bisect-good          (:foreground nightmare-bright_green))
  (magit-log-head-label-bisect-skip          (:foreground nightmare-neutral_yellow))
  (magit-log-head-label-default              (:foreground nightmare-neutral_blue))
  (magit-log-head-label-head                 (:foreground nightmare-light0 :background nightmare-dark_aqua))
  (magit-log-head-label-local                (:foreground nightmare-faded_blue :weight 'bold))
  (magit-log-head-label-patches              (:foreground nightmare-faded_orange))
  (magit-log-head-label-remote               (:foreground nightmare-neutral_blue :weight 'bold))
  (magit-log-head-label-tags                 (:foreground nightmare-neutral_aqua))
  (magit-log-head-label-wip                  (:foreground nightmare-neutral_red))
  (magit-log-message                         (:foreground nightmare-light1))
  (magit-log-reflog-label-amend              (:foreground nightmare-bright_blue))
  (magit-log-reflog-label-checkout           (:foreground nightmare-bright_yellow))
  (magit-log-reflog-label-cherry-pick        (:foreground nightmare-neutral_red))
  (magit-log-reflog-label-commit             (:foreground nightmare-neutral_green))
  (magit-log-reflog-label-merge              (:foreground nightmare-bright_green))
  (magit-log-reflog-label-other              (:foreground nightmare-faded_red))
  (magit-log-reflog-label-rebase             (:foreground nightmare-bright_blue))
  (magit-log-reflog-label-remote             (:foreground nightmare-neutral_orange))
  (magit-log-reflog-label-reset              (:foreground nightmare-neutral_yellow))
  (magit-log-sha1                            (:foreground nightmare-bright_orange))
  (magit-process-ng                          (:foreground nightmare-bright_red :weight 'bold))
  (magit-process-ok                          (:foreground nightmare-bright_green :weight 'bold))
  (magit-section-heading                     (:foreground nightmare-light2 :background nightmare-dark_blue))
  (magit-signature-bad                       (:foreground nightmare-bright_red :weight 'bold))
  (magit-signature-good                      (:foreground nightmare-bright_green :weight 'bold))
  (magit-signature-none                      (:foreground nightmare-faded_red))
  (magit-signature-untrusted                 (:foreground nightmare-bright_purple :weight 'bold))
  (magit-tag                                 (:foreground nightmare-darkslategray4))
  (magit-whitespace-warning-face             (:background nightmare-faded_red))
  (magit-bisect-bad                          (:foreground nightmare-faded_red))
  (magit-bisect-good                         (:foreground nightmare-neutral_green))
  (magit-bisect-skip                         (:foreground nightmare-light2))
  (magit-blame-heading                       (:background nightmare-dark0_soft :foreground nightmare-light0))
  (magit-blame-date                          (:inherit 'magit-blame-heading :foreground nightmare-mid_purple))
  (magit-blame-name                          (:inherit 'magit-blame-heading :foreground nightmare-mid_orange :weight 'bold))
  (magit-blame-hash                          (:inherit 'magit-blame-heading))
  (magit-blame-summary                       (:inherit 'magit-blame-heading :foreground nightmare-dark_purple))
  (magit-sequence-onto                       (:inherit 'magit-sequence-done))
  (magit-sequence-done                       (:inherit 'magit-hash))
  (magit-sequence-drop                       (:foreground nightmare-faded_red))
  (magit-sequence-head                       (:foreground nightmare-faded_cyan))
  (magit-sequence-part                       (:foreground nightmare-bright_yellow))
  (magit-sequence-stop                       (:foreground nightmare-bright_aqua))
  (magit-sequence-pick                       (:inherit 'default))
  (magit-filename                            (:weight 'normal))
  (magit-refname-wip                         (:inherit 'magit-refname))
  (magit-refname-stash                       (:inherit 'magit-refname))
  (magit-refname                             (:foreground nightmare-light2))
  (magit-head                                (:inherit 'magit-branch-local))
  (magit-popup-disabled-argument             (:foreground nightmare-light4))

  ;; MODE SUPPORT: term
  (term-color-black                          (:foreground nightmare-dark1))
  (term-color-blue                           (:foreground nightmare-neutral_blue))
  (term-color-cyan                           (:foreground nightmare-neutral_cyan))
  (term-color-green                          (:foreground nightmare-neutral_green))
  (term-color-magenta                        (:foreground nightmare-neutral_purple))
  (term-color-red                            (:foreground nightmare-neutral_red))
  (term-color-white                          (:foreground nightmare-light1))
  (term-color-yellow                         (:foreground nightmare-neutral_yellow))
  (term-default-fg-color                     (:foreground nightmare-light0))
  (term-default-bg-color                     (:background nightmare-dark0))

  ;; MODE SUPPORT: Elfeed
  (elfeed-search-date-face                   (:foreground nightmare-muted_cyan))
  (elfeed-search-feed-face                   (:foreground nightmare-faded_cyan))
  (elfeed-search-tag-face                    (:foreground nightmare-light3))
  (elfeed-search-title-face                  (:foreground nightmare-light3 :bold nil))
  (elfeed-search-unread-title-face           (:foreground nightmare-light0_hard :bold nil))

  ;; MODE SUPPORT: telephone-line
  (mode-line                                 (:foreground nightmare-light3 :background nightmare-dark1 :box nightmare-dark4))
  (mode-line-inactive                        (:foreground nightmare-medium :background nightmare-dark0_hard :box nightmare-dark0_soft))
  (telephone-line-accent-active              (:foreground nightmare-light0 :background nightmare-dark4))
  (telephone-line-accent-inactive            (:foreground nightmare-medium :background nightmare-dark0_soft))

  ;; MODE SUPPORT: wgrep
  (wgrep-done-face                           (:foreground nil :background nil))
  (wgrep-reject-face                         (:foreground nil :background nil))
  (wgrep-file-face                           (:foreground nil :background nil))
  (wgrep-delete-face                         (:foreground nil :background nil))
  (wgrep-face                                (:foreground nil :background nightmare-dark0_soft))

  ;; MODE SUPPORT: message
  (message-header-to                         (:foreground nightmare-bright_cyan ))
  (message-header-cc                         (:foreground nightmare-bright_cyan ))
  (message-header-subject                    (:foreground nightmare-light2 ))
  (message-header-newsgroups                 (:foreground nightmare-bright_cyan ))
  (message-header-other                      (:foreground nightmare-muted_cyan  ))
  (message-header-name                       (:foreground nightmare-bright_cyan ))
  (message-header-xheader                    (:foreground nightmare-faded_cyan ))
  (message-separator                         (:foreground nightmare-faded_cyan ))
  (message-cited-text                        (:foreground nightmare-light3 ))
  (message-mml                               (:foreground nightmare-faded_aqua )))

 (defface nightmare-modeline-one-active
   `((t
      (:foreground ,nightmare-dark0
                   :background ,nightmare-dark2
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-dark2 :style nil))))
   "nightmare modeline active one")

 (defface nightmare-modeline-one-inactive
   `((t
      (:foreground ,nightmare-dark0
                   :background ,nightmare-dark4
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-dark4 :style nil))))
   "nightmare modeline inactive one")

 (defface nightmare-modeline-two-active
   `((t
      (:foreground ,nightmare-light2
                   :background ,nightmare-dark2
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-dark2 :style nil))))
   "nightmare modeline active two")

 (defface nightmare-modeline-two-inactive
   `((t
      (:foreground ,nightmare-dark1
                   :background ,nightmare-dark4
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-dark4 :style nil))))
   "nightmare modeline inactive two")

 (defface nightmare-modeline-three-active
   `((t
      (:foreground ,nightmare-dark_orange
                   :background ,nightmare-dark0_hard
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-dark0_hard :style nil))))
   "nightmare modeline inactive three")

 (defface nightmare-modeline-three-inactive
   `((t
      (:foreground ,nightmare-muted_orange
                   :background ,nightmare-dark1
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-dark1 :style nil))))
   "nightmare modeline active three")

 (defface nightmare-modeline-four-active
   `((t
      (:foreground ,nightmare-black
                   :background ,nightmare-dark4
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-dark4 :style nil))))
   "nightmare modeline active four")

 (defface nightmare-modeline-four-inactive
   `((t
      (:foreground ,nightmare-dark4
                   :background ,nightmare-black
                   :height 120
                   :inverse-video nil
                   :box (:line-width 6 :color ,nightmare-black :style nil))))
   "nightmare modeline inactive four")

  (custom-theme-set-variables 'nightmare
                             `(pos-tip-foreground-color ,nightmare-light0_hard)
                             `(pos-tip-background-color ,nightmare-dark_aqua)
                             `(ansi-color-names-vector [,nightmare-dark0_soft
                                                        ,nightmare-neutral_red
                                                        ,nightmare-neutral_green
                                                        ,nightmare-neutral_yellow
                                                        ,nightmare-neutral_blue
                                                        ,nightmare-neutral_purple
                                                        ,nightmare-neutral_cyan
                                                        ,nightmare-light1])))

(defun nightmare-modeline-one ()
  "Optional modeline style one for nightmare."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground (face-attribute 'nightmare-modeline-two-active :foreground)
                      :background (face-attribute 'nightmare-modeline-two-active :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-two-active :background) :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-attribute 'nightmare-modeline-two-inactive :foreground)
                      :background (face-attribute 'nightmare-modeline-two-inactive :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-two-inactive :background) :style nil)))

(defun nightmare-modeline-two ()
  "Optional modeline style two for nightmare."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground (face-attribute 'nightmare-modeline-one-active :foreground)
                      :background (face-attribute 'nightmare-modeline-one-active :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-one-active :background) :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-attribute 'nightmare-modeline-one-inactive :foreground)
                      :background (face-attribute 'nightmare-modeline-one-inactive :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-one-inactive :background) :style nil)))

(defun nightmare-modeline-three ()
  "Optional modeline style three for nightmare."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground (face-attribute 'nightmare-modeline-three-active :foreground)
                      :background (face-attribute 'nightmare-modeline-three-active :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-three-active :background) :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-attribute 'nightmare-modeline-three-inactive :foreground)
                      :background (face-attribute 'nightmare-modeline-three-inactive :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-three-inactive :background) :style nil)))

(defun nightmare-modeline-four ()
  "Optional modeline style four for nightmare."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground (face-attribute 'nightmare-modeline-four-active :foreground)
                      :background (face-attribute 'nightmare-modeline-four-active :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-four-active :background) :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-attribute 'nightmare-modeline-four-inactive :foreground)
                      :background (face-attribute 'nightmare-modeline-four-inactive :background)
                      :height 120
                      :inverse-video nil
                      :box `(:line-width 6 :color ,(face-attribute 'nightmare-modeline-four-inactive :background) :style nil)))

(defalias 'nightmare-modeline 'nightmare-modeline-one)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'nightmare)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:

;;; nightmare-theme.el ends here
