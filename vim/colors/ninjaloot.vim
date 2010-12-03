set background=dark
if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif
let g:colors_name="ninjaloot"

hi Boolean         ctermfg=7
hi Comment         ctermfg=8
hi Constant        ctermfg=12
hi DiffAdd         ctermfg=15  ctermbg=4
hi DiffChange                  ctermbg=5
hi DiffDelete      ctermfg=4   ctermbg=6   cterm=none
hi DiffText                    ctermbg=1   cterm=none
hi Directory       ctermfg=6
hi Error           ctermfg=15  ctermbg=1   cterm=none
hi ErrorMsg        ctermfg=15  ctermbg=1   cterm=none
hi FoldColumn      ctermfg=3   ctermbg=none
hi Folded          ctermfg=8   ctermbg=none
hi Function        ctermfg=10
hi Identifier      ctermfg=6               cterm=none
hi Ignore          ctermfg=0
hi IncSearch       ctermfg=3   ctermbg=2   cterm=none
hi LineNr          ctermfg=8
hi MatchParen      ctermfg=9   ctermbg=0
hi ModeMsg         ctermfg=8               cterm=none
hi MoreMsg         ctermfg=2
hi NonText         ctermfg=4               cterm=none
hi PreProc         ctermfg=6
hi Question        ctermfg=2
hi Search          ctermfg=7   ctermbg=12  cterm=none
hi Special         ctermfg=7
hi SpecialKey      ctermfg=0
hi Statement       ctermfg=11
hi StatusLine                  ctermbg=0   cterm=none
hi StatusLineNC    ctermfg=8   ctermbg=0   cterm=none
hi String          ctermfg=9
hi Title           ctermfg=5
hi Type            ctermfg=10
hi Underlined      ctermfg=5               cterm=none
hi VertSplit       ctermfg=0   ctermbg=0   cterm=none
hi Visual                                  cterm=reverse
hi WarningMsg      ctermfg=1
hi WildMenu        ctermfg=0   ctermbg=3

hi Pmenu           ctermfg=8   ctermbg=233
hi PmenuSel                    ctermbg=235
hi PmenuSbar                   ctermbg=232
hi PmenuThumb      ctermfg=16

hi TabLine         ctermfg=3               cterm=none
hi TabLineFill     ctermfg=1               cterm=none
hi TabLineSel      ctermfg=1               cterm=none

if (&term == 'linux' || exists('g:force_lofi') && g:force_lofi > 0)
	hi FoldColumn      ctermfg=3   ctermbg=none
	hi Folded          ctermfg=3   ctermbg=none
	hi VertSplit       ctermfg=5   ctermbg=5   cterm=none
	hi StatusLine      ctermfg=15   ctermbg=5   cterm=none
	hi StatusLineNC    ctermfg=0  ctermbg=5   cterm=none
	hi TabLineSel      ctermfg=15  ctermbg=none    cterm=none
	hi TabLineFill     ctermfg=1  ctermbg=5    cterm=none
	hi TabLine         ctermfg=7 ctermbg=5     cterm=none
elseif &t_Co > 255
    hi CursorLine              ctermbg=233 cterm=none
    hi FoldColumn              ctermbg=233
    hi Folded                  ctermbg=233
    hi Visual                  ctermbg=236 cterm=none
    hi MatchParen              ctermbg=235

    hi Pmenu       ctermfg=8   ctermbg=233
    hi PmenuSel                ctermbg=235
    hi PmenuSbar               ctermbg=232
    hi PmenuThumb  ctermfg=16

    hi TabLine     ctermfg=100 ctermbg=233 cterm=none
    hi TabLineFill ctermfg=233 ctermbg=233 cterm=none
    hi TabLineSel  ctermfg=220 ctermbg=235 cterm=none
end

"vim: sw=4 et
