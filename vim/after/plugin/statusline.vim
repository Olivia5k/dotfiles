" Stallion! The king of statuslines! The statusline of kings!
"
" thx 2 woldrich, scrooloose, matjusushi
"
" stl used for shorter lines

" TODO:
" Full path toggler for %t
" Configuration option implementations
" Configuration keybindings (smart for loop possible?)
" Togglable buffer number?
" Togglable default functions?
" Paste, qf and ll
" Only set colors if not already set

" Zenburn colors! \o/
hi User1 ctermfg=223 ctermbg=234 cterm=bold
hi User2 ctermfg=107 ctermbg=234 cterm=bold
hi User3 ctermfg=238 ctermbg=234 cterm=bold
hi User4 ctermfg=032 ctermbg=234 cterm=bold
hi User5 ctermfg=044 ctermbg=234 cterm=bold
hi User6 ctermfg=197 ctermbg=234 cterm=bold
hi User7 ctermfg=242 ctermbg=234 cterm=bold
hi User8 ctermfg=130 ctermbg=234 cterm=bold
hi User9 ctermfg=065 ctermbg=234 cterm=bold

" ¡Configuraciones!
let g:stl_ff = 1            " NI Fileformat
let g:stl_fenc = 1          " NI File encoding
let g:stl_fugitive = 1      " NI Fugitive
let g:stl_syntax = 1        " NI Syntax groups
let g:stl_spell = 1         " NI Spell language
let g:stl_values = 1        " NI Cursor byte values
let g:stl_ruler = 1         " NI File position data

function! RenderStlFlag(value, good_values, error)
    let good_values = split(a:good_values, ',')
    let good = index(good_values, a:value) != -1
    if (a:error && !good) || (!a:error && good)
        return a:value
    else
        return ''
    endif
endfunction

function! AddStlFlag(var, good_values, true_color, false_color)
    set stl+=%7*[

    exec "set stl+=".a:true_color
    exec "set stl+=%{RenderStlFlag(".a:var.",'".a:good_values."',1)}"
    exec "set stl+=".a:false_color
    exec "set stl+=%{RenderStlFlag(".a:var.",'".a:good_values."',0)}"

    set stl+=%7*]%*
endfunction

function! BufModified(error)
    let bnr = bufnr(bufname("%"))
    let mod =  getbufvar(bnr, "&mod")
    if (a:error && !mod) || (!a:error && mod)
        return bnr
    else
        return ''
    endif
endfunction

function! ColoredBufNr(clean, dirty)
    exec "set stl+=".a:clean
    exec "set stl+=%{BufModified(0)}"
    exec "set stl+=".a:dirty
    exec "set stl+=%{BufModified(1)}"
endfunction

function! StlDelim(delim)
    exec "set stl+=\\ %3*".a:delim."\\ %*"
endfunction

function! StlCondDelim(opt, delim)
    exec "set stl+=%7*%{".a:opt."?'':'".a:delim."'}%*"
endfunction

function! StlCurrentHighlight()
    if exists("g:disable_status_syntax") && g:disable_status_syntax
        return ''
    endif
    return synIDattr(synID(line('.'),col('.'),1),'name')

endfunction
function! StlCurrentRealHighlight()
    if exists("g:disable_status_syntax") && g:disable_status_syntax
        return ''
    endif

    let synId = synID(line('.'),col('.'),1)
    let realSynId = synIDtrans(synId)
    if synId == realSynId
        let realsyn =  'Normal'
    else
        let realsyn =  synIDattr(realSynId, 'name')
    endif
    return realsyn
endfunction

function! StlCache()
    return
endfunction

set stl=
set stl=%1*%t%*
set stl+=%7*[
call ColoredBufNr('%6*', '%2*')
set stl+=%7*]%*
set stl+=%<
call AddStlFlag('&ff', 'unix', '%6*', '%2*')
call AddStlFlag('&fenc', 'utf-8', '%6*', '%2*')
call StlDelim('│')
set stl+=\ %9*%03.3b%*
set stl+=\ %7*\-\ %*
set stl+=%7*0x%8*%02.2B%*
call StlDelim('│')
set stl+=\ %4*%c%7*c%*,
set stl+=\ %*%1*%l%7*/%7*%L%3*
call StlDelim('│')
set stl+=%=
call StlCondDelim('g:disable_status_syntax', '[')
set stl+=%2*%{StlCurrentHighlight()}
call StlCondDelim('g:disable_status_syntax', '/')
set stl+=%6*%{StlCurrentRealHighlight()}
call StlCondDelim('g:disable_status_syntax', ']')
set stl+=%6*
set stl+=%{&paste?'[paste]':''}
set stl+=%{QuickfixCount()}
set stl+=%*
set stl+=%7*
set stl+=%m
call StlDelim('│')

if exists('g:loaded_fugitive')
    set stl+=\ %5*%{fugitive#statusline()}
endif
