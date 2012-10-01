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

function! NonDefaultOptions() " {
    let var = ""

    let var = &wrap == 1               ? var . 'w' : var
    let var = &paste == 1              ? var . 'p' : var
    let var = &spell == 1              ? var . 's' : var
    let var = &spelllang == 'sv'        ? var . 's' : var
    let var = &expandtab == 0          ? var . 't' : var
    let var = &list == 0               ? var . 'L' : var
    let var = &foldmarker != '{,}'     ? var . 'M' : var
    let var = &paste == 0 && &tw != 79 ? var . &tw : var

    return var
endfunction " }

function! SudoWrite() " {
    write !sudo tee %
endfunction " }
function! QuickfixCount() " {
    let qflen = len(getqflist())
    let quick = qflen == 0 ? '' : '['.qflen.'q]'

    let loclen = len(getloclist(0))
    let location = loclen == 0 ? '' : '['.loclen.'l]'

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
function! StatusPathToggle() " {
    if g:show_full_path == 0
        let g:show_full_path = 1
        echo ""
    else
        let g:show_full_path = 0
        echo ""
    endif
endfunction " }

function! RelSwitch(cmd, source, target) " {
    let new = substitute(bufname("%"), a:source, a:target, 'g')
    if filereadable(new)
        execute a:cmd . ' '  . new
    else
        echo "Not a valid file: " . new
    endif
endfunction " }
command! -nargs=+ E :call RelSwitch('edit', <f-args>)
command! -nargs=+ S :call RelSwitch('split', <f-args>)
command! -nargs=+ V :call RelSwitch('vsplit', <f-args>)
command! -nargs=+ T :call RelSwitch('tabe', <f-args>)

function! PySwitchDef(l1, l2)
    let q = "[\"']"
    let obj = '\(\w\+\)\>=\(.\+\)$'
    let dict = q.'\(\w\+\)' .q.':\s\+\(.\+\)$'

    let lines = getline(a:l1, a:l2)
    let processed = []

    for line in lines
        let changed = 0

        if match(line, obj) != -1
            let line = substitute(line, obj, '"\1": \2', '')
            let changed = 1
        elseif match(line, dict) != -1
            let line = substitute(line, dict, '\1=\2', '')
            let changed = 1
        endif

        if changed && match(line, ',$') == -1
            let line = line . ','
        endif

        let processed = add(processed, line)
    endfor

    call setline(a:l1, processed)
endfunction

command! -range -nargs=0 PySwitchDef :call PySwitchDef(<line1>, <line2>)
nmap <silent> <leader>d :PySwitchDef<cr>
vmap <silent> <leader>d :PySwitchDef<cr>gv

" Place at the very bottom since the folding char fucks up the... folding.
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

    " Handle lines that are too long! Cut em down!
    if len(line) + len(fdnfo) > base
        let max = base - len(fdnfo) - 5
        let line = line[:max] . '..{ '
    endif

    let fillcharcount = base - len(line) - len(fdnfo)
    return line . repeat("-",fillcharcount) . fdnfo
endfunction " }
" vim: set et:sw=4:fmr=marker:fdm={,}
