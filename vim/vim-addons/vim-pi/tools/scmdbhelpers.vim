let s:rtp=expand('<sfile>:h:h')
if stridx(&rtp, ','.s:rtp)==-1
    let &rtp.=','.s:rtp
endif
function! GetAuthor()
    wincmd k
    let view=winsaveview()
    .+1? for \d\+:$?+2
    let a=getline('.')
    call winrestview(view)
    wincmd p
    return matchstr(a, 'Author: \zs.*')
endfunction
function! s:GetScriptType(nr)
    let nrnamesdb=vimpi#LoadDBFile('script-id-to-name-log.json')
    let name=get(nrnamesdb, a:nr, [0])[0]
    if name is 0
        return 'utility'
    endif
    let vodb=vimpi#LoadDBFile('vimorgsources.json')
    return get(get(vodb, name, {}), 'script-type', 'utility')
endfunction
function! GetPrevSNR()
    if exists('s:prevsnr')
        let psnr=s:prevsnr
        while psnr%10==9
            let psnr=psnr/10
        endwhile
        if psnr==0
            return ''
        else
            return ''.(psnr/10)
        endif
    else
        return ''
    endif
endfunction
function! s:GitUrl(url)
    if a:url[:23] is# 'https://code.google.com/'
        return substitute(a:url, '/*$', '', '')
    else
        return 'git://'.substitute(substitute(substitute(substitute(a:url, '^\w\+://', '', ''), '/\(/\|$\)\@=', '', ''), '^\([^/]*/[^/]*/[^/]*\).*', '\1', ''), '\.git$', '', '')
    endif
endfunction
function! s:ProcNR(url, nr)
    let nr=a:nr
    if nr is 0 | let nr=substitute(matchstr(a:url, '\v^(\w+\:\/\/)?[^/]+\/[^/]+\/\zs[^/]+'), '\v(\.vim)?(\.git)?/*$', '', '') | endif
    if type(nr)==type(0)
        let s:prevsnr=nr
    endif
    if type(nr)==type(0)
        return 'let scmnr.'.nr.' = '
    else
        return 'let scm['.string(nr).'] = '
    endif
endfunction
function! AddGHUrl(url, nr)
    call append('.', s:ProcNR(a:url, a:nr).
                \    ((a:url=~#'\v\/(blob|raw)\/[^/]+\/.*\.vim$')?
                \       ('vimpi#AddCopyHook({''type'': ''git'', ''url'': '.string(s:GitUrl(a:url)).'}, {'.string(substitute(a:url, '\v^.{-}\/%(blob|raw)\/[^/]+\/(.*)$', '\1', '')).': '.string(vam#utils#GuessFixDir(s:GetScriptType(a:nr))).'})'):
                \    ((a:url=~#'^lp:')?
                \       ('{''type'': ''bzr'', ''url'': '.string(a:url).'}'):
                \    ((a:url=~#'svn')?
                \       ('{''type'': ''svn'', ''url'': '.string(a:url).'}'):
                \    ((a:url=~#'bitbucket\.org' || match(a:url, '\v<hg>')!=-1)?
                \       ('{''type'': ''hg'', ''url'': '.string(a:url).'}'):
                \       ('{''type'': ''git'', ''url'': '.string(s:GitUrl(a:url)).'}'))))))
endfunction
function! AddArchiveUrl(url, nr)
    call append('.', s:ProcNR(a:url, a:nr).
                \   '{''url'': '.string(substitute(a:url, '\V/blob/', '/raw/', '')).', '.
                \    '''archive_name'': '.string(matchstr(a:url, '[^/]\+$')).', '.
                \    '''type'': ''archive'', '.
                \    '''script-type'': '.string(s:GetScriptType(a:nr)).'}')
endfunction
function! GetSNR()
    wincmd k
    let view=winsaveview()
    .+1? for \d\+:$?
    let a=getline('.')
    call winrestview(view)
    wincmd p
    return matchstr(a, '\d\+\ze:$')
endfunction
function! AddAuthor(author)
    if search('\V\^" '.escape(a:author, '\').'\$', 'w')
        normal! ')k
        return
    else
        call append('.', ['', '" '.a:author])
        normal! 2j
    endif
endfunction
function! ScmSourcesFoldExpr(lnum)
    if empty(getline(a:lnum-1))
        return '>1'
    else
        return '='
    endif
endfunction
function! ScmSourcesFoldText(lnum)
    let i=a:lnum+1
    let line=getline(i)
    while line[0] is# '"'
        let i+=1
        let line=getline(i)
    endwhile
    return v:folddashes . ' ' . line
endfunction
function! s:SetFolds()
    setlocal foldexpr=ScmSourcesFoldExpr(v:lnum)
    setlocal foldtext=ScmSourcesFoldText(v:foldstart)
    setlocal foldmethod=expr
endfunction
nnoremap ,gv :call AddGHUrl(@+, +GetSNR())<CR>j
nnoremap ,gg :call AddGHUrl(@+, 0)<CR>j
nnoremap ,ge :call AddGHUrl(@+, )<Left><C-r>=GetPrevSNR()<CR>
nnoremap ,gE :call AddArchiveUrl(@+, )<Left><C-r>=GetPrevSNR()<CR>
nnoremap ,gc :call AddGHUrl(@+, +@*)<CR>
nnoremap ,gC :call AddArchiveUrl(@+, +@*)<CR>
nnoremap ,ga :call AddAuthor(GetAuthor())<CR>
nnoremap ,gA :call AddAuthor(@*)<CR>
nnoremap ,g- :call search('\V"'.repeat('-', 119), 'w')<CR>
nmap     ,gn ,ga,gv
nmap     ,gN ,gA,ge
inoremap ,gs <C-r>=GetSNR()<CR>
nnoremap ,gd olet mai_snr_deps.<C-r>=printf("%-4u", +GetSNR())<CR> = []<Left>
nnoremap ,gD olet add_by_snr.<C-r>=printf("%-4u", +GetSNR())<CR>={'deprecated' : ""}<Left><Left>
nnoremap ,gf :call <SID>SetFolds()<CR>
