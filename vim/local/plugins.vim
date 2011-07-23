source $VIMRUNTIME/macros/matchit.vim
let g:vimwiki_list = [{'path': '~/.local/share/vimwiki/main/', 'path_html': '~/.local/share/vimwiki/html'}]

" Set personal snippet folder location:
let g:xptemplate_snippet_folders=['$HOME/.vim/xptemplate_personal_snippets']
" Turn off automatic closing of quotes and braces:
let g:xptemplate_brace_complete = 0
" Snippet triggering key:
let g:xptemplate_key = '<F1>'
" Open the pop-up menu:
let g:xptemplate_key_pum_only = '<F2>'
" Clear current placeholder and jump to the next:
"imap <C-d> <Tab>
let g:xptemplate_nav_cancel = '<C-d>'
" Move to the next placeholder in a snippet:
let g:xptemplate_nav_next = '<Tab>'
" Go to the end of the current placeholder and in to insert mode:
" <C-_> is actually CONTROL-/ on my keyboard.
let g:xptemplate_to_right = '<C-_>'
" Move cursor back to last placeholder:
let g:xptemplate_goback = '<C-g>'
" Use TAB/S-TAB to navigate through the pop-up menu:
let g:xptemplate_pum_tab_nav = 1
" Reload xptemplate snippets without quitting vim.
nmap <A-F1> :XPTreload<CR>

" vim: set et:sw=4:fmr=marker:fdm={,}
