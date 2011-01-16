" Vim color file
"
" Name:       xoria256.vim
" Version:    1.4
" Maintainer:	Dmitriy Y. Zotikov (xio) <xio@ungrund.org>
"
" Should work in recent 256 color terminals.  88-color terms like urxvt are
" NOT supported.
"
" Don't forget to install 'ncurses-term' and set TERM to xterm-256color or
" similar value.
"
" Color numbers (0-255) see:
" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html

" Initialization 
if &t_Co != 256 && ! has("gui_running")
  echomsg ""
  echomsg "err: please use GUI or a 256-color terminal (so that t_Co=256 could be set)"
  echomsg ""
  finish
endif

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "mancx"

" Colours {1 
"" General {2
hi Normal       ctermfg=252 guifg=#d0d0d0 ctermbg=234 guibg=#1c1c1c cterm=bold gui=none
hi Cursor                                 ctermbg=214 guibg=#ffaf00
hi CursorColumn                           ctermbg=238 guibg=#444444
hi ColorColumn                            ctermbg=235
hi CursorLine                             ctermbg=238 guibg=#444444 cterm=bold
hi Directory    ctermfg=76 guifg=Cyan                              cterm=bold
hi Error        ctermfg=15  guifg=#ffffff ctermbg=1   guibg=#800000
hi ErrorMsg     ctermfg=15  guifg=#ffffff ctermbg=1   guibg=#800000
hi FoldColumn   ctermfg=58 guifg=#9e9e9e ctermbg=233 guibg=#101010 cterm=bold
hi Folded       ctermfg=58 guifg=#eeeeee ctermbg=233  guibg=#5f5f87
hi IncSearch    ctermfg=0   guifg=#000000 ctermbg=223 guibg=#ffdfaf cterm=bold gui=none
hi LineNr       ctermfg=247 guifg=#9e9e9e ctermbg=233 guibg=#101010
hi MatchParen   ctermfg=188 guifg=#dfdfdf ctermbg=68  guibg=#5f87df cterm=bold gui=bold
hi NonText      ctermfg=247 guifg=#9e9e9e ctermbg=233 guibg=#101010 cterm=bold gui=bold
hi Pmenu        ctermfg=0   guifg=#000000 ctermbg=246 guibg=#949494
hi PmenuSbar                              ctermbg=243 guibg=#767676
hi PmenuSel     ctermfg=0   guifg=#000000 ctermbg=243 guibg=#767676
hi PmenuThumb                             ctermbg=252 guibg=#d0d0d0
hi Search       ctermfg=0   guifg=#000000 ctermbg=149 guibg=#afdf5f
hi SignColumn   ctermfg=248 guifg=#a8a8a8
hi SpecialKey   ctermfg=22  guifg=#5fdf5f
hi SpellBad     ctermfg=160 guifg=fg      ctermbg=bg                cterm=underline               guisp=#df0000
hi SpellCap     ctermfg=189 guifg=#d0d0ff ctermbg=bg  guibg=bg      cterm=underline gui=underline 
hi SpellRare    ctermfg=168 guifg=#d75f87 ctermbg=bg  guibg=bg      cterm=underline gui=underline 
hi StatusLine   ctermfg=154               ctermbg=239 guibg=#4e4e4e cterm=bold gui=bold
hi StatusLineNC ctermfg=246               ctermbg=239 guibg=#3a3a3a cterm=none gui=none
hi TabLine      ctermfg=246  guifg=fg      ctermbg=239 guibg=#666666 cterm=bold gui=none
hi TabLineFill  ctermfg=fg  guifg=fg      ctermbg=239 guibg=#4e4e4e cterm=bold gui=none
hi Title        ctermfg=225 guifg=#ffdfff
hi Todo         ctermfg=0   guifg=#000000 ctermbg=184 guibg=#dfdf00
hi Underlined   ctermfg=34  guifg=#00afff                           cterm=none gui=underline
hi VertSplit    ctermfg=239 guifg=#4e4e4e ctermbg=239 guibg=#4e4e4e cterm=bold gui=none
hi Visual       ctermfg=255 guifg=#eeeeee ctermbg=96  guibg=#5f875f
hi VisualNOS    ctermfg=255 guifg=#eeeeee ctermbg=60  guibg=#5f5f87
hi WildMenu     ctermfg=0   guifg=#000000 ctermbg=150 guibg=#afdf87 cterm=bold gui=bold

"" Syntax highlighting {2
hi Comment      ctermfg=244 guifg=#808080
hi Constant     ctermfg=64 guifg=#ffffaf
hi Exception    ctermfg=94 guifg=#dfafdf                           cterm=bold
hi Identifier   ctermfg=113 guifg=#dfafdf                           cterm=bold
hi Ignore       ctermfg=238 guifg=#444444
hi Number       ctermfg=76 guifg=#dfaf87
hi Operator     ctermfg=78 guifg=#afdf87 cterm=bold
hi PreProc      ctermfg=156 guifg=#afdf87 cterm=bold
hi Special      ctermfg=94 guifg=#df8787
hi Statement    ctermfg=76 guifg=#87afdf                           cterm=bold gui=none
hi Repeat       ctermfg=34 guifg=#87afdf                           cterm=bold gui=none
hi Todo         ctermfg=196 guifg=#afafdf ctermbg=none              cterm=bold gui=none
hi Type         ctermfg=71 guifg=#afafdf                           cterm=bold gui=none

hi htmlItalic ctermfg=101 ctermbg=none

"" Special {2
""" .diff {3
hi diffAdded    ctermfg=150 guifg=#afdf87
hi diffRemoved  ctermfg=174 guifg=#df8787
""" vimdiff {3
hi diffAdd      ctermfg=bg  guifg=bg      ctermbg=151 guibg=#afdfaf
"hi diffDelete   ctermfg=bg  guifg=bg      ctermbg=186 guibg=#dfdf87 cterm=bold gui=none
hi diffDelete   ctermfg=bg  guifg=bg      ctermbg=246 guibg=#949494 cterm=bold gui=none
hi diffChange   ctermfg=bg  guifg=bg      ctermbg=181 guibg=#dfafaf
hi diffText     ctermfg=bg  guifg=bg      ctermbg=174 guibg=#df8787 cterm=bold gui=none


" vim: set expandtab tabstop=2 shiftwidth=2 smarttab softtabstop=2:
