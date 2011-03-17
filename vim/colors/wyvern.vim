"
" Vim colour file
"
" Maintainer:  Vy-Shane Sin Fat <shane@node.mu>
" Last Change: 20 November 2009
" Version:     1.1
"
" This colour file is meant for GUI use.
"

set background=light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="wyvern"


hi Normal        guifg=#1a1a1a  gui=bold
hi Title         guifg=black    guibg=white
hi Cursor        guibg=#111111
hi LineNr        guifg=#aaaaaa  guibg=#f8f8f8
hi Visual        guibg=#bbddff
hi NonText       guifg=#cccccc  guibg=#f5f5f5
hi StatusLine    guifg=#222222  guibg=#cccccc  gui=bold
hi StatusLineNC  guifg=#666666  guibg=#cccccc  gui=none
hi VertSplit     guifg=#eeeeee  guibg=#eeeeee  gui=none
hi ModeMsg       guifg=#007050  guibg=#eeeeee  gui=none
hi ErrorMsg      guifg=#f03050  guibg=#eeeeee  gui=none
hi Error         guifg=#bb3355  guibg=white    gui=none


" Vim 7.x specific
if version >= 700
  hi CursorLine  guibg=#eeeeee  gui=none
  hi MatchParen  guibg=#ccffdd  gui=none
  hi Pmenu       guifg=#60656f  guibg=#f0f5ff  gui=none
  hi PmenuSel    guifg=white    guibg=#3585ef  gui=bold
  hi PmenuSbar   guifg=#d0d5dd  guibg=#e0e5ee  gui=bold
  hi PmenuThumb  guifg=#e0e5ee  guibg=#c0c5dd  gui=bold
  hi Search      guibg=#fcfcaa  gui=none
  hi IncSearch   guibg=#ffff33  gui=bold
endif


" Syntax highlighting 
hi Comment       guifg=#668866  gui=none
"hi Todo          guifg=#225522  guibg=white    gui=italic
hi Todo          guifg=#ff0000  gui=bold
hi Operator      guifg=#1a1a1a  gui=bold
hi Identifier    guifg=#1a1a1a  gui=bold
hi Statement     guifg=#0050b0  gui=bold
hi Type          guifg=#0050b0  gui=bold
hi Constant      guifg=#204070  gui=bold
hi Conditional   guifg=#004040  gui=bold
hi Delimiter     guifg=#1a1a1a  gui=bold
hi PreProc       guifg=#007050  gui=bold
hi Special       guifg=#a05050  gui=bold
hi Keyword       guifg=#007050  gui=bold

hi link Function        Normal
hi link Character       Constant
hi link String          Constant
hi link Boolean         Constant
hi link Number          Constant
hi link Float           Number
hi link Repeat          Conditional
hi link Label           Statement
hi link Exception       Statement
hi link Include         PreProc
hi link Define          PreProc
hi link Macro           PreProc
hi link PreCondit       PreProc
hi link StorageClass    Type
hi link Structure       Type
hi link Typedef         Type
hi link Tag             Special
hi link SpecialChar     Special
hi link SpecialComment  Special
hi link Debug           Special

