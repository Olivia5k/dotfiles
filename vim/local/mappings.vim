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
    "noremap <return> zA

    " Reselect the visuals when indenting
    vnoremap < <gv
    vnoremap > >gv
" }
" Sourcing {
    nmap <Leader>v :source $MYVIMRC<cr>
    nmap <Leader>V :tabe $MYVIMRC<cr>
    nmap <Leader>t :NERDTreeToggle<cr><C-w>=
    nmap <Leader>x :!xclip %<cr>
    nmap <Leader>r :call ReloadAllSnippets()<cr>
" }
" Togglers {
    nmap <Leader>w :call WrapToggle()<cr>
    nmap <Leader>p :call PasteToggle()<cr>
    nmap <Leader>l :call SpellToggle()<cr>
    nmap <Leader>C :call ColorschemeToggle()<cr>
    nmap <Leader>y :call StatusSyntaxToggle()<cr>
    nmap <Leader>s mt:call WhitespaceToggle()<cr>`t
    nmap <Leader>S mt:call KillTrailingWhitespace(0)<cr>`t
    nnoremap <silent> <C-l> :nohl<CR><C-l>
" }
" Fugitive <3 {
    nmap gi :Git 
    nmap gb :Gblame<cr>
    nmap gc :Gcommit<cr>
    nmap gd :Gdiff 
    nmap gdd :Gdiff 
    nmap gdc :Gdiff<cr>
    nmap gdh :Gdiff HEAD<cr>
    nmap gl :Extradite<cr>
    nmap go :Git checkout 
    nmap gpp :Git push origin 
    nmap guu :Git pull origin 
    nmap gre :Gread 
    nmap grr :Gremove
    nmap gs :Gstatus<cr>
    nmap gw :Gwrite<cr>
    nmap ga :Gwrite<cr>
    nmap gm :Gmove 
" }
" Misc. {
    "com! W call SudoWrite()
    " Show syntax groups beneath cursor
    " TODO: Make function
    nmap <Leader>g  :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
    nmap gpb o{<cr>}<esc>k

    " Search for all single/double-quoted strings
    nmap <Leader>hs /\('.\{-}'\\|".\{-}"\)<cr>
    nmap <Leader>m :make<cr>
    nmap <Leader>f gggqG
    nmap <Leader>a ,lj

    "sunmap s
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

    nmap ch :cp<CR>
    nmap cl :cn<CR>
    nmap cj :cc<CR>
" }
" Frustration {
    nmap K k
    vmap K k
    nmap S S<esc>
    noremap Q gq
    nnoremap Y y$
    nnoremap ; :
" }

" vim: set et:sw=4:fmr=marker:fdm={,}
