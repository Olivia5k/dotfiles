" Control
"nmap E <esc>/<++><cr>4s
"nmap <C-J> <esc>/<++><cr>4s
"imap <C-J> <esc>/<++><cr>4s

" Starting
nmap gppe o?><esc>
nmap gpps o<?php<esc>
nmap gppE O?><esc>
nmap gppS O<?php<esc>
nmap gppp o<?php<cr>?><esc>k
nmap gppP O<?php<cr>?><esc>k

" Unechoing - To HTML
nmap gph ^df"$hD:s/\\"/"<cr>:s/\\n//<cr>^

" Debug and undebug
nmap gpdb odebug();<esc>hi
nmap gpdq odebug($q);<esc>^
nmap gpde odebug($link->error);<esc>^
nmap gpdd :g/^\s*debug\(.*\);$/d<cr>

" Variables
nmap gpva T$ea['']<esc>hi
nmap gpve f]a['']<esc>hi
nmap gpvg a$_GET['']<esc>hi
nmap gpvp a$_POST['']<esc>hi
nmap gpvs a$_SESSION['']<esc>hi
nmap gpvS a$_SERVER['']<esc>hi
nmap gpvi o$x = isset($_GET['x']) ? $_GET['x'] : "";<esc>:s/x/

" Functions
nmap gpi oif()<esc>i
nmap gpfo ofor(;<++>;<++>)<esc>T(i
nmap gpfe oforeach( as $<++>)<esc>T(i
nmap gpfE oforeach( as $<++> => $<++>)<esc>T(i
nmap gpw owhile()<esc>i
nmap gpb o{<cr>}<esc>k
nmap gpB O{<esc>jo}<esc>k

" Appending
nmap gpae /}<cr>oelse<cr>{<cr>}<esc>O
nmap gpai /}<cr>oelse if()<cr>{<cr><++><cr>}<esc>3k8li
nmap gpc ?{<cr>ddjddk^

" MySQL
nmap gpmq o$q = "";<cr>$link->query($q);<esc>^k2f"i
nmap gpmi o$q = "";<cr>$res = $link->query($q);<cr><cr>if($res && $res->num_rows <++>)<cr>{<cr><++><cr>}<esc>6k2f"i
nmap gpmw o$q = "";<cr>$res = $link->query($q);<cr><cr>if($res && $res->num_rows <++>)<cr>{<cr>while($row = $res->fetch_object())<cr>{<cr><++><cr>}<cr>}<esc>9k2f"i
nmap gpmr o$row = $res->fetch_object();<esc>^

nmap gpmes F$i$link->real_escape_string(<esc>lea)
nmap gpmei F$i(int)$link->real_escape_string(<esc>lea)

" Visual
vmap gpe "pdi<?php echo <esc>mha<C-R>p; ?><esc>
nmap gpe F$vegpe

" Linewise
nmap gP I<?php <esc>A ?><esc>^<esc>

" Custom
nmap gplui a$_SESSION['thisuser']->getusrid()<esc>
nmap gplug a$_SESSION['thisuser']->getgrpids()<esc>
nmap gpluG a$_SESSION['thisuser']->getgrpnames()<esc>
nmap gplus a$_SESSION['thisuser']->getsecrethash()<esc>

nmap gpls oif($_['secrethash'] == <esc>gplusa)<esc>2T(la
nmap gplr oreport("","<++>",<++>);<esc>T(a
nmap gplg oif(in_array("",<esc>gpluGa))<cr>{<cr><++><cr>}<esc>3k2f"i

nmap gpt mt:%s/    /\t<cr>`t

"nmap ,p :source ~/.vim/plugin/php.vim<cr>
"nmap ,P :sp ~/.vim/plugin/php.vim<cr>

" Notes
" Names to remember MySQL functions are; gpm + query/row/if/while
"
