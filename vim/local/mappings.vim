let mapleader = ","
let g:EasyMotion_leader_key = '<Leader>m'

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

    " Parenthesis entry
    inoremap §1 ()<ESC>i
    inoremap §2 []<ESC>i
    inoremap §3 {<ESC>o}<ESC>O
    inoremap §4 {}<ESC>i
    inoremap §Q ""<ESC>i
    inoremap §q ''<ESC>i
    nnoremap d= f=d$a=
    nnoremap d> f>d$a>
" }
" Sourcing {
    nmap <Leader>sv :source $MYVIMRC<cr>
    nmap <Leader>sV :tabe $MYVIMRC<cr>
    nmap <Leader>t :NERDTreeToggle<cr><C-w>=
    nmap <Leader>x :!xclip %<cr>
    nmap <Leader>r :call ReloadAllSnippets()<cr>
    nmap <Leader>g :GundoToggle<cr>
" }
" Togglers {
    nmap <Leader>sw :call WrapToggle()<cr>
    nmap <Leader>p :call PasteToggle()<cr>
    nmap <Leader>sl :call SpellToggle()<cr>
    nmap <Leader>sy :call StatusSyntaxToggle()<cr>
    nmap <Leader>sf :call StatusPathToggle()<cr>
    nmap <Leader>s mt:call WhitespaceToggle()<cr>`t
    nmap <Leader>S mt:call KillTrailingWhitespace(0)<cr>`t
    nnoremap <silent> <C-l> :nohl<CR><C-l>
" }
" Fugitive <3 {
    " Run any git command from inside vim
    nmap gi :Git 
    nmap gb :Gblame<cr>
    nmap gC :Gcommit<cr>
    nmap gdd :Gdiff 
    nmap gdc :Gdiff<cr>
    nmap gdh :Gdiff HEAD<cr>
    " Check previous revisions of current file; git log
    nmap gl :Extradite<cr>
    nmap go :Git checkout 
    nmap gpp :Git push origin 
    nmap guu :Git pull origin 
    nmap gre :Gread 
    nmap grr :Gremove
    " Use - on files to add or reset them. Use C to go to commit
    nmap gs :Gstatus<cr>
    nmap gw :Gwrite<cr>
    nmap ga :Gwrite<cr>
    nmap gm :Gmove 
" }
" Misc. {
    nmap gpb o{<cr>}<esc>k

    " Search for all single/double-quoted strings
    nmap <Leader>hs /\('.\{-}'\\|".\{-}"\)<cr>

    nmap <Leader>m :make<cr>
    nmap <Leader>q ,lb
    nmap <Leader>a ,lj
    nmap <Leader>z ,lf

    nnoremap <TAB> %
" }
" Turbo editing {
    cnoremap %% <C-R>=expand('%:h').'/'<cr>
    map <leader>ee :e %%
    map <leader>es :sp %%
    map <leader>ev :vsp %%
    map <leader>et :tabe %%
" }
" Folding {
    nmap <Leader>fm :set foldmethod=marker<cr>
    nmap <Leader>fi :set foldmethod=indent<cr>
" }
" CSS {
    nmap <Leader>b A {<cr>}<esc>k^
" }
" Indentation {
    " Indent XML
    nmap <Leader>ix :%s/>/>\r/<cr>:%s/</\r</<cr>gg=G:g/^$/d<cr>
" }
" Better tabs for the hands {
    nmap zh gT
    nmap zl gt
    nmap zn :tabe 
    nmap zH :tab he 

    " Quickfix mappings. Mnemonic is for 'change' or 'cc'
    nmap ch :cp<CR>
    nmap cj :cc<CR>
    nmap ck :cw<CR>
    nmap cl :cn<CR>
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
