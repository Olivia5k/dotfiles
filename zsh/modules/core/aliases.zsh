alias grep="grep $GREPOPTS"
alias :q="exit"
alias bell='echo -en "\007"'

if has alsamixer ; then
	alias am='alsamixer'
fi

if has todo ; then
	alias t='todo'
	alias tt='todo +children'
fi

if has fetchmail ; then
	alias fm='fetchmail'
fi

if has python ; then
	alias py='python'
	if has bpython ; then
		alias bp='bpython' # <3
	fi
fi

# rsync should always be timed, nuff said.
if has rsync ; then
	alias rsync='time rsync'
fi

if has vlock ; then
	alias lock='vlock -n'
fi

# X11 specific aliases
if [ -n "$DISPLAY" ] ; then
	# Sets your keyboard to be snappier. Very recommended.
	alias xr='xset r rate 330 45 && echo 330/45'

	# Sets custom keymaps of both qwerty and dvorak
	alias xa='setxkbmap a6'
	alias xq='setxkbmap q6'
	alias aoeu="xq"
	alias asdf="xa"

	if has urxvt ; then
		alias u='urxvt &| exit'
		alias us='urxvt -name smallfont &| exit'
	fi
fi

# Count which commands you use the most. Give a numerical argument and it
# will print a list of that length.
# The aliases module is the best suited place for this one, really.
function lscmd()
{
	if [ -n "$1" ] ; then
		c=$1
	else
		c=25
	fi

	cat $LOGS/zsh.history.log |
	perl -pe 's/^: [0-9]+:[0-9]+;\s*//' |
	sort |
	uniq -c |
	sort -n -k1 |
	tail -$c |
	tac
}
