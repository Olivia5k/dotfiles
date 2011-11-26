let mapleader = ","

" Navigation {
    " Unmap them evil arrows
    noremap <Up> <NOP>
    noremap <Right> <NOP>
    noremap <Down> <NOP>
    noremap <Left> <NOP>
    inoremap <Up> <NOP>
    inoremap <Right> <NOP>
    inoremap <Down> <NOP>
    inoremap <Left> <NOP>

    " Better use of commons
    noremap <space> /

    " Reselect the visuals when indenting
    vnoremap < <gv
    vnoremap > >gv

    " Easier escaping!
    inoremap jj <ESC>
    inoremap jk <ESC>
" }
" Sourcing {
    nmap <Leader>sv :source $MYVIMRC<cr>
    nmap <Leader>sV :tabe $MYVIMRC<cr>
    nmap <Leader>x :!xclip %<cr>
" }
" Togglers {
    nmap <Leader>sw :call WrapToggle()<cr>
    nmap <Leader>p :call PasteToggle()<cr>
    nmap <Leader>sl :call SpellToggle()<cr>
    nmap <Leader>sy :call StatusSyntaxToggle()<cr>
    nmap <Leader>sf :call StatusPathToggle()<cr>
    nmap <Leader>s mt:call WhitespaceToggle()<cr>`t
    nnoremap <silent> <C-l> :nohl<CR><C-l>
" }
" Misc. {
    " Search for all single/double-quoted strings
    nmap <Leader>hs /\('.\{-}'\\|".\{-}"\)<cr>
    nmap <Leader>m :make<cr>
" }
" Turbo switching {
    " Expands %% into the directory containing the current buffer!
    cnoremap %% <C-R>=expand('%:h').'/'<cr>
    nmap <leader>ee :e %%
    nmap <leader>es :sp %%
    nmap <leader>ev :vsp %%
    nmap <leader>et :tabe %%
    nmap <leader>eV :tabe ~/config/vim/
" }
" Folding {
    nmap <Leader>fm :set foldmethod=marker<cr>
    nmap <Leader>fi :set foldmethod=indent<cr>
" }
" Indentation {
    " Indent XML
    nmap <Leader>ix :%s/>/>\r/<cr>:%s/</\r</<cr>gg=G:g/^$/d<cr>
" }
" Better for the hands {
    nmap zh gT
    nmap zl gt
    exec "nmap zn :tabe "
    exec "nmap zH :tab he "

    " loclist mappings.
    nmap ch :lpe<CR>
    nmap cj :ll<CR>
    nmap ck :lw<CR>
    nmap cl :lne<CR>

    " Same, but for quickfix
    nmap cqh :cp<CR>
    nmap cqj :cc<CR>
    nmap cqk :cw<CR>
    nmap cql :cn<CR>

    " Diffing
    nmap dn ]c
    nmap dN [c
" }
" Frustration {
    nmap K k
    vmap K k
    nmap S S<esc>
    noremap Q q:
    nnoremap Y y$
    nnoremap ; :
" }

" vim: set et:sw=4:fmr=marker:fdm={,}
