runtime autoload/vimpi.vim

redir => s:funcs
" Do not make it indent: use :execute to hide :function
silent execute 'function /^vimpi#'
redir END

for s:fname in map(split(s:funcs, "\n"), 'matchstr(v:val, ''\v%(vimpi#)@<=[^(]*'')')
    if !empty(s:fname)
        execute         "function vamkr#".s:fname."(...)"
                    \."\n    echohl WarningMsg"
                    \."\n    echomsg 'Function vamkr#".s:fname." is deprecated, use vimpi#".s:fname."'"
                    \."\n    echohl None"
                    \."\n    return call('vimpi#".s:fname."', a:000)"
                    \."\nendfunction"
    endif
endfor
