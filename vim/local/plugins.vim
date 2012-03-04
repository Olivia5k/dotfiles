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
    " As alot of these bindings end with a space, they are wrapped in an exec
    " To avoid the unavoidable mistake of accidentally clearing them with the
    " autocommand that clears trailing whitespace.
    " To be consistent, all of them are wrapped, even if they don't have the
    " trailing space.

    exec "nmap gi :Git "
    exec "nmap gb :Gblame<cr>"
    exec "nmap gcc :Gcommit<cr>"
    exec "nmap gca :Gcommit --amend<cr>"
    exec "nmap gdd :Gdiff "
    exec "nmap gdc :Gdiff<cr>"
    exec "nmap gdh :Gdiff HEAD<cr>"

    " Check previous revisions of current file; git log
    exec "nmap gle :Extradite<cr>"
    exec "nmap gll :Glog<cr>"
    exec "nmap glo :Glog "
    exec "nmap go :Git checkout "
    exec "nmap gpp :Git push origin "
    exec "nmap guu :Git pull origin "
    exec "nmap gre :Gread "
    exec "nmap grr :Gremove "

    " Use - on files to add or reset them. Use C to go to commit
    exec "nmap gs :Gstatus<cr>"
    exec "nmap gw :Gwrite<cr>"
    exec "nmap ga :Gwrite<cr>"
    exec "nmap gm :Gmove "

    " Buffer cleanup
    autocmd BufReadPost fugitive://* set bufhidden=delete
" }
" XPTemplate {
    " let g:xptemplate_snippet_folders=['$HOME/.vim/xpt']
    let g:xptemplate_brace_complete = 0  " Delimitmate, bitches
    let g:xptemplate_key = 'ö'  " Main snippet key
    let g:xptemplate_key_pum_only = '<C-F>'  " Popup key
    let g:xptemplate_nav_cancel = '<C-D>'  " Cancel key
    let g:xptemplate_nav_next = '<Tab>'  " Next key
    let g:xptemplate_goback = '<S-Tab>'  " Prev key
    let g:xptemplate_to_right = '<C-L>'  " Exit key
    let g:xptemplate_pum_tab_nav = 1  " Tab navigation in popup menu

    " Easy reload
    nmap <leader>r :XPTreload<CR>

    fun! XPTedit() "{{{
        let filetypes = split(&filetype, '.')

        " noremap \fx :exec 'e ~/.vim/xpt-personal/ftplugin/'.&filetype.'/'.&filetype.'.xpt.vim'<cr>
    endfunction "}}}
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
    if executable('jsl')
        let g:syntastic_javascript_checker = 'jsl'
        let g:syntastic_javascript_jsl_conf = '-conf ~/.vim/local/conf/jsl.conf'
    endif
" }
" Powerline {
    let Powerline_theme = 'distinguished'
    " let g:Powerline_symbols = 'compatible'
    let Powerline_cachefile = ""
" }

" Helper plugins
" Lusty {
    " Dat silence
    let g:LustyJugglerSuppressRubyWarning = 1
" }
" CtrlP {
    " Quick bindings
    nmap <A-d> :CtrlP<cr>
    nmap <Leader>q :CtrlP<cr>
    nmap <Leader>a :CtrlPBuffer<cr>
    let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
" }
" delimitMate {
    let g:delimitMate_expand_space = 1
    let g:delimitMate_expand_cr = 1
" }
" commentary {
    " Backslash is horrible on swedish layouts. ö is not.
    " However, ö is now the main xpt trigger key, which conflicts here in
    " visual mode. Experiment with the more traditional #.
    nmap ö \\\
    vmap # \\\
" }
" vimwiki {
    let g:vimwiki_list = [{'path': '~/.local/share/vimwiki/main/', 'path_html': '~/.local/share/vimwiki/html'}]
" }
" yankring {
    let g:yankring_history_file = ".cache/vim/tmp/yankring"
    nmap zp :YRShow<cr>
    nmap zs :YRSearch<cr>
" }
" surround {
    " While I love surround.vim, the s/S confusion in visual mode does not
    " really tickle my fancy. To make up for this, these mappings happened.
    vmap " S"
    vmap ' S'
    vmap t St
    vmap ( S(
    vmap [ S[
    vmap { S{
"

" vim: set et:sw=4:fdm=marker:fmr={,}
