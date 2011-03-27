" Modded zenburn that better fits 256 color terminals
" Edited by Lowe Thiderman (daethorian@ninjaloot.se)

" Mostly, the colorscheme is stripped down to only hi statements. The original
" zenburn had really weird setup for the terminal with non-lapsing if conditions
" and colors that hardly matched the original.

" Minor adjustments have been made, mostly bolding of major keywords and
" corrections of stuff that was outright wrong (white background for visual
" selection? really?). Also, to make the statements readable, they were set in
" columns that always are the very same. In the original, bg and fg were mixed
" and watnot.

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="zenburnt"

hi Boolean         ctermfg=181
hi Character       ctermfg=181                 cterm=bold
hi ColorColumn                   ctermbg=238
hi Comment         ctermfg=108
hi Conditional     ctermfg=223                 cterm=bold
hi Constant        ctermfg=181                 cterm=bold
hi Cursor                        ctermbg=109
hi Cursor          ctermfg=233   ctermbg=109   cterm=bold
hi CursorColumn                  ctermbg=239   cterm=none
hi CursorLine                    ctermbg=238   cterm=none
hi Debug           ctermfg=181                 cterm=bold
hi Define          ctermfg=223                 cterm=bold
hi Delimiter       ctermfg=245
hi DiffAdd                       ctermbg=065
hi DiffChange                    ctermbg=239
hi DiffDelete      ctermfg=174   ctermbg=236
hi DiffText                      ctermbg=236   cterm=underline
hi Directory       ctermfg=188                 cterm=bold
hi Error           ctermfg=228   ctermbg=95
hi ErrorMsg        ctermfg=115   ctermbg=236   cterm=bold
hi Exception       ctermfg=249                 cterm=bold
hi Float           ctermfg=251
hi FoldColumn      ctermfg=109   ctermbg=236
hi Folded          ctermfg=109   ctermbg=238
hi Function        ctermfg=228                 cterm=bold
hi Identifier      ctermfg=223
hi Ignore          ctermfg=240
hi IncSearch       ctermfg=108   ctermbg=234   cterm=reverse
hi Include         ctermfg=223                 cterm=bold
hi Keyword         ctermfg=223                 cterm=bold
hi Label           ctermfg=187                 cterm=underline
hi LineNr          ctermfg=248   ctermbg=235
hi Macro           ctermfg=223                 cterm=bold
hi ModeMsg         ctermfg=223                 cterm=none
hi NonText         ctermfg=242   ctermbg=236
hi Normal          ctermfg=188   ctermbg=237
hi Number          ctermfg=116
hi Operator        ctermfg=230
hi PMenu           ctermfg=248   ctermbg=0
hi PMenuSel        ctermfg=223   ctermbg=235
hi PreCondit       ctermfg=180                 cterm=bold
hi PreProc         ctermfg=223                 cterm=bold
hi Question        ctermfg=15                  cterm=bold
hi Repeat          ctermfg=223                 cterm=bold
hi Search          ctermfg=230   ctermbg=65
hi Special         ctermfg=181
hi SpecialChar     ctermfg=181                 cterm=bold
hi SpecialComment  ctermfg=108                 cterm=bold
hi SpecialKey      ctermfg=240
hi SpellBad        ctermfg=196                 cterm=underline
hi SpellCap        ctermfg=174
hi SpellLocal      ctermfg=174
hi SpellRare       ctermfg=174
hi Statement       ctermfg=187   ctermbg=237   cterm=bold
hi StatusLine      ctermfg=236   ctermbg=186
hi StatusLineNC    ctermfg=235   ctermbg=108
hi StorageClass    ctermfg=249                 cterm=bold
hi String          ctermfg=174
hi Structure       ctermfg=229                 cterm=bold
hi TabLine         ctermfg=248   ctermbg=235   cterm=bold
hi TabLineFill                   ctermbg=235   cterm=none
hi TabLineSel      ctermfg=223   ctermbg=237   cterm=bold
hi Tag             ctermfg=181                 cterm=bold
hi Title                         ctermbg=237
hi Todo            ctermfg=228   ctermbg=234   cterm=bold
hi Type            ctermfg=187                 cterm=bold
hi Typedef         ctermfg=253                 cterm=bold
hi Underlined      ctermfg=188   ctermbg=234   cterm=bold
hi VertSplit       ctermfg=235   ctermbg=65
hi Visual                        ctermbg=234
hi VisualNOS                     ctermbg=234
hi WarningMSG                    ctermbg=236
hi WarningMsg      ctermfg=15    ctermbg=236   cterm=bold
hi WildMenu        ctermfg=194   ctermbg=236   cterm=bold
