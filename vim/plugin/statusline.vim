finish
let g:statusline_max_path = 20
fun! StatusLineGetPath() "{{{
  let p = expand('%:.:h') "relative to current path, and head path only
  let p = substitute(p,'\','/','g')
  let p = substitute(p, '^\V' . $HOME, '~', '')
  if len(p) > g:statusline_max_path 
    let p = simplify(p)
    let p = pathshorten(p)
  endif
  return p
endfunction "}}}

nmap <Plug>view:switch_status_path_length :let g:statusline_max_path = 200 - g:statusline_max_path<cr>
nmap ,t <Plug>view:switch_status_path_length



set laststatus=2

augroup Statusline
  au! Statusline

  au BufEnter * call <SID>SetFullStatusline()
  au BufLeave,BufNew,BufRead,BufNewFile * call <SID>SetSimpleStatusline()
augroup END

fun! StatusLineRealSyn()
    let synId = synID(line('.'),col('.'),1)
    let realSynId = synIDtrans(synId)
    if synId == realSynId
        return 'Normal'
    else
        return synIDattr( realSynId, 'name' )
    endif
endfunction

fun! s:SetFullStatusline() "{{{
  setlocal statusline=
  setlocal statusline+=%#StatuslineBufNr#%-1.2n\                   " buffer number
  setlocal statusline+=%h%#StatuslineFlag#%m%r%w                 " flags
  setlocal statusline+=%#StatuslinePath#\ %-0.20{StatusLineGetPath()}%0* " path
  setlocal statusline+=%#StatuslineFileName#\/%t\                       " file name

  setlocal statusline+=%#StatuslineFileEnc#\ %{&fileencoding}\         " file encoding
  setlocal statusline+=%#StatuslineFileType#\ %{strlen(&ft)?&ft:'**'}\ . " filetype
  setlocal statusline+=%#StatuslineFileType#%{&fileformat}\             " file format
  setlocal statusline+=%#StatuslineTermEnc#\ %{&termencoding}\           " encoding
  setlocal statusline+=%#StatuslineChar#\ %-2B\ %0*                 " current char
  setlocal statusline+=%#StatuslineSyn#\ %{synIDattr(synID(line('.'),col('.'),1),'name')}\ %0*           "syntax name
  setlocal statusline+=%#StatuslineRealSyn#\ %{StatusLineRealSyn()}\ %0*           "real syntax name
  setlocal statusline+=%=

  setlocal statusline+=\ %-10.(%l,%c-%v%)             "position
  setlocal statusline+=%P                             "position percentage
  setlocal statusline+=\ %#StatuslineTime#%{strftime(\"%m-%d\ %H:%M\")} " current time

endfunction "}}}

fun! s:SetSimpleStatusline() "{{{
  setlocal statusline=
  setlocal statusline+=%#StatuslineNC#%-0.20{StatusLineGetPath()}%0* " path
  setlocal statusline+=\/%t\                       " file name
endfunction "}}}

