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
