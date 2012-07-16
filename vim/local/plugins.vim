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
    exec "nmap ge :Gedit<cr>"
    exec "nmap gb :Gblame<cr>"
    exec "nmap gc :Gcommit<cr>"
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
    let g:xptemplate_key = '<Tab>'  " Main snippet key
    let g:xptemplate_key_pum_only = '<C-F>'  " Popup key
    let g:xptemplate_nav_cancel = '<C-D>'  " Cancel key
    let g:xptemplate_nav_next = '<C-J>'  " Next key
    let g:xptemplate_goback = '<C-K>'  " Prev key
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
    let loaded_css_syntax_checker = 1

    if executable('jsl')
        let g:syntastic_javascript_checker = 'jsl'
        let g:syntastic_javascript_jsl_conf = '-conf ~/.vim/local/conf/jsl.conf'
    endif
    let g:syntastic_javascript_jslint_conf = "--maxerr 1000 --white --undef --nomen --regexp --plusplus --bitwise --newcap --sloppy --vars --continue"
" }
" Powerline {
    let Powerline_theme = 'default'
    let Powerline_colorscheme = 'default'
    " let g:Powerline_symbols = 'compatible'
    let Powerline_cachefile = ""
" }
" Python mode {
    " Set key 'R' for run python code
    let g:pymode_run_key = 'R'

    " Load show documentation plugin
    let g:pymode_doc = 1
    " Key for show python documentation
    let g:pymode_doc_key = 'K'

    " Load run code plugin
    let g:pymode_run = 0

    " Key for run python code
    let g:pymode_run_key = '<leader>pr'

    " Load pylint code plugin
    let g:pymode_lint = 0
    " Switch pylint, pyflakes, pep8, mccabe code-checkers
    " Can have multiply values "pep8,pyflakes,mcccabe"
    let g:pymode_lint_checker = "pyflakes"
    " Skip errors and warnings
    " E.g. "E501,W002", "E2,W" (Skip all Warnings and Errors startswith E2) and etc
    let g:pymode_lint_ignore = "E501"
    " Select errors and warnings
    " E.g. "E4,W"
    let g:pymode_lint_select = ""
    " Run linter on the fly
    let g:pymode_lint_onfly = 0
    " Pylint configuration file
    " If file not found use 'pylintrc' from python-mode plugin directory
    let g:pymode_lint_config = "$HOME/.pylintrc"
    " Check code every save
    let g:pymode_lint_write = 0
    " Auto open cwindow if errors be finded
    let g:pymode_lint_cwindow = 0
    " Show error message if cursor placed at the error line
    let g:pymode_lint_message = 1
    " Auto jump on first error
    let g:pymode_lint_jump = 0
    " Hold cursor in current window
    " when quickfix is open
    let g:pymode_lint_hold = 0
    " Place error signs
    let g:pymode_lint_signs = 1
    " Minimal height of pylint error window
    let g:pymode_lint_minheight = 3
    " Maximal height of pylint error window
    let g:pymode_lint_maxheight = 6

    " Load rope plugin
    let g:pymode_rope = 1
    " Auto create and open ropeproject
    let g:pymode_rope_auto_project = 1
    " Enable autoimport
    let g:pymode_rope_enable_autoimport = 1
    " Auto generate global cache
    let g:pymode_rope_autoimport_generate = 0
    let g:pymode_rope_autoimport_underlineds = 0
    let g:pymode_rope_codeassist_maxfixes = 10
    let g:pymode_rope_sorted_completions = 1
    let g:pymode_rope_extended_complete = 1
    let g:pymode_rope_autoimport_modules = ["os","shutil","datetime"]
    let g:pymode_rope_confirm_saving = 1
    let g:pymode_rope_global_prefix = "<C-q>p"
    let g:pymode_rope_local_prefix = "<C-q>r"
    let g:pymode_rope_vim_completion = 1
    let g:pymode_rope_guess_project = 1
    let g:pymode_rope_goto_def_newwin = 0
    let g:pymode_rope_always_show_complete_menu = 0

    " Load python objects and motion
    let g:pymode_motion = 1
    " Load breakpoints plugin
    let g:pymode_breakpoint = 0
    " Key for set/unset breakpoint
    let g:pymode_breakpoint_key = '<leader>b'
    " Autoremove unused whitespaces
    let g:pymode_utils_whitespaces = 0
    " Auto fix vim python paths if virtualenv enabled
    let g:pymode_virtualenv = 0
    " Set default pymode python indent options
    let g:pymode_options_indent = 1
    " Set default pymode python fold options
    let g:pymode_options_fold = 1
    " Set default pymode python other options
    let g:pymode_options_other = 1

    " Enable pymode's custom syntax highlighting
    let g:pymode_syntax = 1
    " Enable all python highlightings
    let g:pymode_syntax_all = 1
    " Highlight "print" as function
    let g:pymode_syntax_print_as_function = 0
    " Highlight indentation errors
    let g:pymode_syntax_indent_errors = g:pymode_syntax_all
    " Highlight trailing spaces
    let g:pymode_syntax_space_errors = g:pymode_syntax_all
    " Highlight string formatting
    let g:pymode_syntax_string_formatting = g:pymode_syntax_all
    " Highlight str.format syntax
    let g:pymode_syntax_string_format = g:pymode_syntax_all
    " Highlight string.Template syntax
    let g:pymode_syntax_string_templates = g:pymode_syntax_all
    " Highlight doc-tests
    let g:pymode_syntax_doctests = g:pymode_syntax_all
    " Highlight builtin objects (__doc__, self, etc)
    let g:pymode_syntax_builtin_objs = g:pymode_syntax_all
    " Highlight builtin functions
    let g:pymode_syntax_builtin_funcs = g:pymode_syntax_all
    " Highlight exceptions
    let g:pymode_syntax_highlight_exceptions = g:pymode_syntax_all
    " For fast machines
    let g:pymode_syntax_slow_sync = 0
" }

" Helper plugins
" CtrlP {
    " Quick bindings
    nmap <A-d> :CtrlP<cr>
    nmap <Leader>q :CtrlP<cr>
    nmap <Leader>a :CtrlPBuffer<cr>
    let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
    let g:ctrlp_root_markers = ['.git']
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
    vmap ö \\\
" }
" NERDTree {
    nmap ,t :NERDTree<cr><C-W>=
" }
" vimwiki {
    let g:vimwiki_list = [{'path': '~/.local/share/vimwiki/main/', 'path_html': '~/.local/share/vimwiki/html'}]
    let g:vimwiki_folding = 1
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
    vmap ( S)
    vmap [ S]
    vmap { S}
" }
" unimpaired {
    nmap <A-j> ]e
    nmap <A-k> [e

    vmap <A-j> ]egv
    vmap <A-k> [egv
" }
" vimroom {
    nnoremap <silent> <Leader>mz <Plug>VimroomToggle
" }

" vim: set et:sw=4:fdm=marker:fmr={,}
