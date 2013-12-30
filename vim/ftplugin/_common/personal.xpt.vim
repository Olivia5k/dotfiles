" Move me to your own fptlugin/_common and config your personal information.
"
" Here is the place to set personal preferences; "priority=personal" is the
" highest which overrides any other XPTvar setting.
"
" You can also set personal variables with 'g:xptemplate_vars' in your .vimrc.
XPTemplate priority=personal


XPTvar $author       'Lowe Thiderman'
XPTvar $email        'lowe.thiderman@gmail.com'
XPTvar $username     'thiderman'

XPTvar $SPfun ''
XPTvar $SParg ''
XPTvar $SPcmd ''
XPTvar $SPop ' '

XPT gh " Github URL
https://github.com/`username^/`repo^

XPT xprio
XPTemplate priority=personal
