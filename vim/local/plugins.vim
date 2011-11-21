" General vim plugins
" Matchit {
    source $VIMRUNTIME/macros/matchit.vim
" }
" Sanity {
    " The retard that set C-C as default for SQL completion should get his face
    " caved in.
    let g:ftplugin_sql_omni_key = '<C-Q>'
" }

" Larger cool plugins
" Fugitive {
    " Run any git command from inside vim
    nmap gi :Git 
    nmap gb :Gblame<cr>
    nmap gc :Gcommit<cr>
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

    " Buffer cleanup
    autocmd BufReadPost fugitive://* set bufhidden=delete
" }
" XPTemplate {
    let g:xptemplate_snippet_folders=['$HOME/.vim/xpt']
    let g:xptemplate_brace_complete = 0  " Delimitmate, bitches
    let g:xptemplate_key = '<C-J>'  " Main snippet key
    let g:xptemplate_key_pum_only = '<C-F>'  " Popup key
    let g:xptemplate_nav_cancel = '<C-D>'  " Cancel key
    let g:xptemplate_nav_next = '<Tab>'  " Next key
    let g:xptemplate_goback = '<S-Tab>'  " Prev key
    let g:xptemplate_to_right = '<C-L>'  " Exit key
    let g:xptemplate_pum_tab_nav = 1  " Tab navigation in popup menu

    " Easy reload
    nmap <leader>r :XPTreload<CR>
" }
" Sparkup {
    " Sparkup bindings that don't mess with other existing bindings
    let g:sparkupExecuteMapping = '<C-E>'
    let g:sparkupNextMapping = '<C-Q>'
" }
" Rope {
    " Smarter bindings
    au FileType python nnoremap <buffer> <cr> :RopeGotoDefinition<cr>
    au FileType python nnoremap <buffer> <bs> :RopeShowDoc<cr>
" }
" Syntastic {
    let g:syntastic_enable_signs = 1
    let g:loaded_html_syntax_checker = 1
    let g:syntastic_jslint_conf = ' --continue'
" }

" Helper plugins
" Lusty {
    " Quick bindings
    nmap <Leader>q ,lb
    nmap <Leader>a ,lj
    nmap <Leader>z ,lf
" }
" delimitMate {
    let g:delimitMate_expand_space = 1
    let g:delimitMate_expand_cr = 1
" }
" commentary {
    " Backslash is horrible on swedish layouts. ö is not.
    nmap ö \\\
    vmap ö \\\
" }

" Random
" vimwiki {
    let g:vimwiki_list = [{'path': '~/.local/share/vimwiki/main/', 'path_html': '~/.local/share/vimwiki/html'}]
" }

" vim: set et:sw=4:fmr=marker:fdm={,}
