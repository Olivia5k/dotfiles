function! FoldWithLines() " {
    let nucolwidth = &fdc + &number*&numberwidth
    let winwd = winwidth(0) - nucolwidth - 5
    let foldlinecount = foldclosedend(v:foldstart) - foldclosed(v:foldstart) + 1
    let fdnfo = " " . string(foldlinecount) . " lines "
    let line = strpart(getline(v:foldstart), 0 , winwd - len(fdnfo)) . " "
    if &tw
        let base = &tw
    else
        let base = 79
    endif
    let fillcharcount = base - len(line) - len(fdnfo)
    return line . repeat("-",fillcharcount) . fdnfo
endfunction " }

function! WrapToggle() " {
    if &wrap == 0
        set nolist wrap linebreak
        echo 'Wrap enabled'
    else
        set list nowrap nolinebreak
        echo 'Wrap disabled'
    endif
endfunction " }
function! PasteToggle() " {
    if &paste == 0
        set paste
        echo 'Paste enabled'
    else
        set nopaste
        echo 'Paste disabled'
    endif
endfunction " }
function! SpellToggle() " {
    if &spelllang == "en"
        set spelllang=sv
        echo 'Spell set to swedish'
    else
        set spelllang=en
        echo 'Spell set to english'
    endif
endfunction " }
function! WhitespaceToggle() " {
    if &expandtab == 0
        set expandtab
        try
            silent :%s/\t/    /
        catch /E486:/
        endtry
        echo 'Whitespace set to space'
    else
        set noexpandtab
        try
            silent :%s/    /\t/
        catch /E486:/
        endtry
        echo 'Whitespace set to tabs'
    endif
endfunction " }
function! ColorschemeToggle() " {
    if g:colors_name == 'xoria'
        colorscheme wyvern
        hi Normal ctermbg=none
    else
        colorscheme xoria
    endif
endfunction " }
function! LoFiToggle() " {
    if g:force_lofi == 1
        let g:force_lofi = 0
        echo 'Lofi disabled'
    else
        let g:force_lofi = 1
        echo 'Lofi enabled'
        silent colorscheme ninjaloot
    endif
endfunction " }

function! KillTrailingWhitespace(write_file) " {
    try
        :%s/\s\+$//
        echo 'Trailing whitespace eliminated'
    catch /E486:/
        echo 'No trailing whitespace'
    endtry

    if a:write_file == 1
        write
    endif
endfunction " }
function! SudoWrite() " {
    write !sudo tee %
endfunction " }
function! QuickfixCount() " {
    let qflen = len(getqflist())
    let quick = qflen == 0 ? '' : '['.qflen.'cc]'

    let loclen = len(getloclist(0))
    let location = loclen == 0 ? '' : '['.loclen.'ll]'

    return quick . location
endfunction " }
function! StatusSyntaxToggle() " {
    if g:disable_status_syntax == 0
        let g:disable_status_syntax = 1
        echo 'Syntax shower disabled'
    else
        let g:disable_status_syntax = 0
        echo 'Syntax shower enabled'
    endif
endfunction " }

function! RelSwitch(source, target) " {
    let new = substitute(bufname("%"), a:source, a:target, 'g')
    if filereadable(new)
        edit `=new`
    else
        echo "Not a valid file: " . new
    endif
endfunction " }
command! -nargs=+ S :call RelSwitch(<f-args>)

" vim: set et:sw=4:fmr=marker:fdm={,}
