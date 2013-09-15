" This was written since most colorschemes can't really handle using bolds the
" way I want bolds to be used; at all.

let s:groups = [
  \ 'Function', 'PreProc', 'Statement', 'String', 'Number', 'Special',
  \ 'Comment', 'Identifier'
  \ ]

for group in s:groups
  exec "hi ".group." cterm=bold gui=bold"
endfor

" Also, I always want these highlighted the same way
syn match Braces display '[{}()\[\]]'
hi link Braces Comment
