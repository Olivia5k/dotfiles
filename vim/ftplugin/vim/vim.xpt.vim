XPTemplate priority=personal

XPTvar $SPindent '  '

XPT section wrap " Section header
" `section^ {{{

`cursor^

" }}}
..XPT

XPT fun wrap " fun! ..(..) .. endfunction
function! `name^`$SPfun^(`:_args:^)
`$SPindent`cursor^
endfunction

XPT augroup wrap
augroup `name^
`$SPindent`cursor^
augroup END

XPT line
let line = getline('.')

XPT fnm
fnamemodify(`cursor^)

XPT plugin
" plugin/`file()^
" Author:       `$author^ <`$email^>
" Github:       https://github.com/`username^/vim-`project^

if exists('g:loaded_`project^') || &cp || v:version < 700
  finish
endif
let g:loaded_`project^ = 1

let s:cpo_save = &cpo
set cpo&vim

`cursor^

let &cpo = s:cpo_save
..XPT

XPT autoload
" autoload/`file()^
" Author:       `$author^ <`$email^>
" Github:       https://github.com/`username^/vim-`project^

if exists('g:autoloaded_`project^')
  finish
endif
let g:autoloaded_`project^ = '`0.1^'

let s:cpo_save = &cpo
set cpo&vim

`cursor^

let &cpo = s:cpo_save
..XPT
