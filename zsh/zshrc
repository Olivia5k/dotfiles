# zshrc by Lowe Thiderman (daethorian@ninjaloot.se)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).
#
# <github link>

# Compiled with bits and pieces from all over the place.  Notable sources are the
# zsh manpages, zsh lovers, Phil!'s prompt and the Arch Linux BBS.

# http://grml.org/zsh/zsh-lovers.html
# http://aperiodic.net/phil/prompt/
# https://bbs.archlinux.org

# Loading and setting {{{
	setopt extendedglob
	umask 022

	# While vim is superior, shells in vi mode are unfortunately not.
	bindkey -e

	autoload colors zsh/terminfo # Colors!
	autoload -Uz vcs_info # git and svn integration

	autoload -Uz url-quote-magic # URL escaping
	zle -N self-insert url-quote-magic
# }}}
# The debugger {{{
	zdebug() {
		if [ -n "$DEBUG" ] && $DEBUG ; then
			print -P "%B%F{cyan}Debug%f%b:" $*
		fi
	}
	zerror() {
		print -P "%B%F{red}Error%f%b:" $*
	}
# }}}
# Variables {{{
	# zsh configuration directory
	ZSHCONF="$HOME/config/zsh"
	# File with variables that most probably changes per user.
	# Most documentation about this configuration is in there.
	local USERFILE="$ZSHCONF/$USER.conf"
	local LOCALFILE="$ZSHCONF/zshlocal"

	if [ -f $USERFILE ] ; then
		source $USERFILE
	else
		source $LOCALFILE
	fi

	# Prompt variables
	export TCOLORS=$(echotc Co)

	# Helper function and setter
	has() {
		if which $1 &> /dev/null ; then
			echo true
		else
			echo false
		fi
	}

	if [ -n "$MULTI" ] ; then
		export HASMULTI=$(has $MULTI)
	else
		export HASMULTI=false
	fi
	if [ -n "$TODO" ] ; then
		export HASTODO=$(has $TODO)
	else
		export HASTODO=false
	fi

	#export PYTHONSTARTUP=$HOME/.pystartup
	export PYTHONSTARTUP=$HOME/.bpystartup

	if [ $TERM = "linux" ] && $FORCE_CONSOLE; then
		export PMODE=1
	elif [ $TERM = "xterm" ] && $FORCE_MOBILE ; then
		export PMODE=0
	fi

	# Store last prompt; for use with CVS prompt
	export POLD=$PMODE

	# Kill root after three minutes
	if [ "$UID" = 0 ] && [ -n "$ROOT_TIMEOUT" ] ; then
		print -P "Warning: Root shell will timeout after %B%F{red}180 seconds%f%b."
		TMOUT=$ROOT_TIMEOUT
	fi
# }}}
# Directories {{{
	for d in $HOMEBIN $LOGS $ZDUMPDIR ; do
		if ! [ -d $d ] ; then
			zdebug "Autocreating %B%F{blue}${d}%b%f"
			mkdir -p $d &> /dev/null
		fi
	done
# }}}
# History {{{
	mkdir -p $LOGS 

	HISTFILE="$LOGS/zsh.history.log"
	HISTSIZE=100000
	SAVEHIST=100000
# }}}
# Completion {{{
	zstyle :compinstall filename '~/.zshrc'

	autoload -Uz compinit
	compinit -d $COMPDUMP

	# TODO: Add decorator format function with prompt lookalikes

	# allow approximate matching
	zstyle ':completion:*' completer _complete _match _approximate
	zstyle ':completion:*:match:*' original only
	zstyle ':completion:*:approximate:*' max-errors 2 numeric
	zstyle ':completion:*:descriptions' format "%B%F{green}%d%f%b:"
	zstyle ':completion:*:directories' format "%B%F{blue}%d%f%b:"
	zstyle ':completion:*:corrections' format "%B%F{red}%d%b%f:" # (%B%F{red}%e%f%b errors):"
	zstyle ':completion:*:default' list-prompt '%S%M matches%s'
	zstyle ':completion:*' group-name ''
	zstyle ':completion:*:manuals' separate-sections true
	zstyle ':completion:*:manuals.(^1*)' insert-sections true
	zstyle ':completion:*' menu select
	zstyle ':completion:*' verbose yes
	zstyle ':completion:*' use-cache on
	zstyle ':completion:*' cache-path $ZDOTDIR/zcompcache

	# tab completion for PID :D
	zstyle ':completion:*:*:kill:*' menu yes select
	zstyle ':completion:*:kill:*' force-list always

	# Make cd not select parent dir
	zstyle ':completion:*:cd:*' ignore-parents parent pwd

# }}}

# General aliases {{{
	alias grep="grep $GREPOPTS"
	alias vim="vim -p"
	alias t='todo'
	alias tt='todo +children'
	alias dt='dmesg'
	alias fm='fetchmail'
	alias py='python'
	alias bp='bpython' # <3
	alias pl='perl'
	alias am='alsamixer'
	alias xr='xset r rate 330 45'
	alias xa='setxkbmap a6'
	alias xq='setxkbmap q6'
	alias ms="rsync $REMOTE:mail/ ~/mail -a --delete &> /dev/null"
	alias :q="exit"
	alias aoeu="xq"
	alias asdf="xa"

	alias u='urxvt &| exit'
	alias us='urxvt -name smallfont &| exit'
	alias uz='uzbl-browser'
	alias ut='uzbl-tabbed'

	alias lock='vlock -n'
	alias bell='echo -en "\007"'
	alias rsync='time rsync'
	alias shutdown='sudo shutdown -h now'
	alias reboot='sudo reboot'

	lscmd() {
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
# }}}
# Extendning {{{
	cl() { cd $1 && ls }
	cll() { cd $1 && ls -l }
	clal() { cd $1 && ls -al }

	# grep processes and retain grep color
	psg() {
		ps haux | grep -i $1 | grep -Ev '(grep)' | grep -i $1
	}
# }}}
# ls and files {{{
	ls="ls $LSOPTS"
	alias ls="$ls"
	alias lc="$ls --color=never"
	alias ll="$ls -l"
	alias la="$ls -a"
	alias lal="$ls -al"
	alias lla="$ls -Al"

	alias lsd="$ls -d *(-/N)"           # list visible directories
	alias lsf="$ls *(-.N)"              # list visible files

	alias la="$ls -A"                   # list all files/directories
	alias lad="$ls -d *(-/DN)"          # list all directories
	alias laf="$ls -A *(-.DN)"          # list all files
	alias llad="$ls -lhd *(-/DN)"       # list details of all directories
	alias llaf="$ls -lhA *(-.DN)"       # list details of all files
	alias lld="$ls -lhd *(-/N)"         # list details of visible directories
	alias llf="$ls -lh *(-.N)"          # list details of visible files
	alias lh="$ls -d .*"                # list hidden files/directories
	alias lhd="$ls -d .*(-/N)"          # list hidden directories
	alias lhf="$ls .*(-.N)"             # list hidden files
	alias llh="$ls -lhd .*"             # list details of hidden files/directories
	alias llhd="$ls -lhd .*(-/N)"       # list details of hidden directories
	alias llhf="$ls -lh .*(-.N)"        # list details of hidden files

	alias le="$ls -d *(-/DN^F)"         # list all empty directories
	alias ler="$ls -d **/*(-/DN^F)"     # list all empty directories recursively
	alias lle="$ls -ld *(-/DN^F)"       # list details of all empty directories
	alias ller="$ls -lhd **/*(-/DN^F)"  # list details of all empty directories recursively

	swap() { # Swap name of $1 and $2
		if [ -f $1 ] ; then
			if [ -f $2 ] ; then
				mv $1 $1.swapcopy;
				mv $2 $1;
				mv $1.swapcopy $2;
			else
				echo "'$2' is not a valid file!"
			fi
		else
			echo "'$1' is not a valid file!"
		fi
	}
	mk() {
		mkdir $* && cd $1
	}
	rmext() {
		for e in $*; do
			find . -iname "*.$e" | xargs rm -rv
		done
	}
# }}}
# Dotdot {{{
	alias '..'='cd ..'
	alias -g '...'='../..'
	alias -g '....'='../../..'
	alias -g '.....'='../../../..'
# }}}
# Finding and grepping {{{
	# Find a file.
	f() { # find a file
		find 2>/dev/null | grep -is "$1"
	}

	# Todo grep. Prints reminding lines that developers leave in their code.
	# (gt is taken by git stash)
	tg() {
		grep '(TODO|XXX|FIXME)' * -ERIin
	}

	# Global grep. Searches files for content.
	gg() {
		grep "$*" * -RIins
	}

	# Same as above, but does not print the matching line. Useful when searching
	# through files with criminally long lines.
	ggl() {
		grep "$*" * -RlIis
	}

	# List differential files between two directories
	dirdiff() {
		if [ -d $1 ] && [ -d $1 ] ; then
			_dirdiff $1 $2
			_dirdiff $2 $1
		else
			zerror "Both arguments must be valid directories"
		fi
	}
	_dirdiff() {
		local listfile="/tmp/.zshlistfile"
		find $2 -type f > $listfile

		print -P "\n%BFiles in %F{blue}${1}%f not in %F{blue}${2}%f%b:"
		for i in $(find $1 -type f) ; do
			f=$(basename $i)
			if ! grep $f $listfile &> /dev/null; then
				echo $i
			fi
		done
		rm $listfile
	}

	grab() { # change mode and users of files to safe default
		chmod -R u=rwX,go=rX "$@"
		chown -R ${USER}:users "$@"
	}
	sgrab() {
		sudo chmod -R u=rwX,go=rX "$@"
		sudo chown -R ${USER}:users "$@"
	}
	wgrab() { # change mode and users of files to warez
		sudo chmod -R 775 "$@"
		sudo chown -R root:warez "$@"
	}
# }}}
# Global aliases {{{
	alias -g G="| grep -i"
	alias -g M="| most"
	alias -g X="| xargs"
	alias -g H="| head"
	alias -g T="| tail"
	alias -g S="| sort"
	alias -g SU='| sort -u'
	alias -g SN='| sort -n'
	alias -g SNR='| sort -nr'
	alias -g W="| wc -l"
	alias -g E="&| exit"
	alias -g N="&> /dev/null"
	alias -g 1N="1> /dev/null"
	alias -g 2N="2> /dev/null"
# }}}
# LOLCODE!! {{{
	lolcode()
	{
		ln=$(grep -n LOLCODE ~/.zshrc)
		num=$(echo $ln | grep -Eo '[[:digit:]]+')
		h=$(echo $num | head -n 1)
		b=$(echo $num | tail -n 1)
		(( t = h - b - 1))

		head -n $b ~/.zshrc | tail -n $t
	}
	alias wtf='dmesg'
	alias onoz='sudo cat /var/log/errors.log'
	alias rtfm='man'

	alias visible='echo'
	alias invisible='cat' # :D
	alias moar='more'

	alias icanhas='mkdir'
	alias donotwant='rm'
	alias dowant='cp'
	alias gtfo='mv'

	alias hai='cd'
	alias plz='pwd'

	alias nomz='ps -aux'
	alias nomnom='killall'

	alias cya='sudo reboot'
	alias kthx='exit'
	alias kthxbai='sudo shutdown -h now'
# }}} END LOLCODE
# ssh {{{
	alias n="ssh $REMOTE"
	alias nl='ssh 10.0.0.6'
	alias pp='ssh root@pepper'
	alias pg='ssh git@pepper'
# }}}
# Django {{{
	alias dm='python2 manage.py'
	alias dr='dm runserver'
	alias ds='echo "no" | dm syncdb'
	alias dz='bpython'

	# Django sync hard. Useful when you update a model and the regular sync
	# cant catch it. Note that extensive fixtures are crucial for this to be
	# useful.
	dsh() {
		for db in `mysql -s -s -e "show tables in dev_main"` ; do
			mysql -e "drop table dev_main.$db"
		done
		
		ds
	}
# }}}
# Terminal multiplexing {{{
	if $(has screen) ; then
		alias sr='screen -r'
		alias sw='screen -wipe'
	fi

	if $(has tmux) ; then
		alias ta='tmux attach'
		alias td='tmux detach'
		alias tr='td && ta'
	fi
# }}}
# Revision handling {{{
	if $(has svn); then
		alias sp='svn up' # su is... yeah.
		alias sc='svn ci -m ""'
		alias rmsvn='find . -name "*.svn" -type d | xargs rm -rv'
	fi
	if $(has git); then
		alias ga='git add'
		alias gb='git branch'
		alias gc='git commit -a'
		alias gC='git commit -a && git push'
		alias gd='git diff'
		alias gdt='git difftool'
		alias gl='git log --pretty=oneline'
		alias gL='git log'
		alias go='git checkout'
		alias gp='git push'
		alias gs='git status'
		alias gst='git stash'
		alias gt='git tag'
		alias gU='git pull && git submodule update'
		alias gu='git pull' # git update...-ish.

		# Setup default master options
		alias gm='git config branch.master.remote origin &&
				git config branch.master.merge refs/heads/master' 
		alias gau='git update-index --assume-unchanged'
		gi() {
			if [ -z "$1" ] ; then ; echo "Specify project name" && exit 1 ; fi

			git init $1
			cd $1
			touch README
			git add README
			git commit -m "Initial commit"
		}
	fi
# }}}
# System and service handling {{{
	d() { # control daemons: d <daemon> <action>
	if [ "$UID" = 0 ];then
		/etc/rc.d/$1 $2
	else
		sudo /etc/rc.d/$1 $2
	fi
	}
	nr() { # control wireless network
		if [ -n "$1" ] ; then
			wlan=$1
		else
			wlan='home'
		fi

		sudo netcfg -d $wlan
		sudo netcfg $wlan
	}
	alias ia='sudo ifconfig -a'
	alias ar='sudo /etc/init.d/apache2 restart'
# }}}
# MPD {{{
	if $(has mpd); then
		export MPD_HOST=localhost
		export MPD_PORT=6600

		alias np='mpc --format "%position%) %artist% #[%album%#] - %title%"' # Now playing
		alias mps='np toggle' # Stop
		alias mpl='np playlist'
		alias mpa='mpc add'
		alias mpx='mpc clear'
		alias mpll='mpc load' # Playlist load
		alias mpls='mpx && mpll' # Playlist switch
		alias mpp='np play'
		alias mpr='np random'

		if $(has mpcext) ; then
			alias mpg='mpcext -s'
			alias mpag='mpcext -S' # grep all
			alias mpgs='mpcext -sw' # grep switch
			alias mpq='mpcext -q' # Queue
		fi
	fi
# }}}
# Mounting {{{
	# USB, yeah
	alias mmu='mount /mnt/usb'
	alias muu='umount /mnt/usb'

	# Warez
	alias mmw='sshfs ninjaloot.se:/warez /mnt/warez -o allow_other'
	alias mmwl='sshfs 10.0.0.6:/warez /mnt/warez -o allow_other'
	alias muw='fusermount -u /mnt/warez'

	# sshfs home
	alias mmn='sshfs ninjaloot.se: ~/ssh/ninjaloot'
	alias mmnl='sshfs 10.0.0.6: ~/ssh/ninjaloot'
	alias mun='fusermount -u ~/ssh/ninjaloot'

	# Force unmounting
	alias muf='sudo umount -l /mnt/warez && sudo umount -l ~/ssh/ninjaloot'
# }}}
# Failsafe! :D {{{
	fail() {
		if which figlet &> /dev/null && [ $PMODE != 0 ]; then
			# If figlet exists and we are not on a mobile connection, doit
			figlet :@ && echo
		else
			# Print red angry text
			echo -e "\n\033[1;31m   YOU'RE DOING IT WRONG! :@\033[0m\n"
		fi
	}

	alias sl='fail && ls'
	alias ös='fail && ls'
	alias lös='fail && ls'
	alias xs='fail && cd'
	alias vf='fail && cd'
	alias cd..='fail && cd ..'
	alias vom='fail && vim'
	alias bim='fail && vim'
	alias shitdown='fail && shutdown' # :D
# }}}
# Configuration and sourcing {{{
	# Easy editing. \o/
	alias ea="$EDITOR ~/config/awesome/rc.lua"
	alias er="$EDITOR ~/config/ratpoison/ratpoisonrc"
	alias es="$EDITOR ~/config/stumpwm/stumpwmrc"
	alias et="$EDITOR ~/config/tmux/tmux.conf"
	alias ev="$EDITOR ~/config/vim/vimrc"
	alias ew="$EDITOR ~/config/wmii/wmii.conf"
	alias ex="$EDITOR ~/config/x/Xdefaults"
	alias exi="$EDITOR ~/config/x/xinitrc"
	alias ez="$EDITOR ~/config/zsh/zshrc"
	alias ezl="$EDITOR ~/config/zsh/zshlocal"
	alias z=ez # \o/

	# Sourcing
	alias aa='awesome -k'
	alias zz="source ~/.zshrc"

	# Project directories
	alias cdc='cd ~/config'
	alias cdg='cd ~/git'
	alias cdr='cd ~/git/rls'
	alias cdn='cd ~/git/ndev'
	alias cdm='cd ~/git/django-mancx'
	alias cdp='cd /usr/lib/python2.7/site-packages/'
	alias cdu='cd /warez/unpack' # onoes

	# Print a file without comments
	alias cc='grep -Ev "^\s*(#|$)"'

# }}}
# Colorscheme printing {{{
	# Prints a table of the 16 default colors. Usually comes as a standalone
	# script, but hey, why not integrate it? :>
	c() {
		T='HaX' # The test text
		echo -e "                 40m     41m     42m     43m     44m     45m     46m     47m";

		for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
			'1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
			'  36m' '1;36m' '  37m' '1;37m';
		do
			FG=${FGs// /}
			echo -en " $FGs \033[$FG  $T  "
			for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
				do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
			done
			echo;
		done
	}

	# Prints a table of the 256 extended colors. Incredibly useful when making
	# terminal editor colorschemes. The default prints quite wide, so when in a
	# small terminal, it prints a simplified version.
	C() {
		if [ $(stty size | cut -f2 -d\ ) -ge 90 ] ; then
			c='Color '
		else
			c=''
		fi

		for i in {16..255}; do
			echo -en "\e[38;5;${i}m$c $i\t"
			[ $((++j%6)) -eq 0 ] && echo
		done
	}
# }}}
# File installation {{{
	# Install an executable into $HOMEBIN
	ins() {
		f=$(readlink -f $1)
		if [ -f $f ] && [ -x $f ] ; then
			if [ -n "$2" ] ; then
				local target=$2
			else
				local target=$(basename $1)
			fi

			if ! $(has $target) ; then
				print -P "Installing %B%F{green}$(basename $1)%f%b as %B%F{green}${target}%f%b"
				ln -s $f $HOMEBIN/$target
			else
				zerror "$target is already installed"
			fi
		else
			zerror "$(basename $f) is not an executable file"
		fi
	}
	
	# Remove an executable installed by ins()
	unins() {
		f=$HOMEBIN/$(basename $1)
		if [ -L $f ] ; then
			print -P "Uninstalling %B%F{green}$(basename $1)%f%b."
			unlink $f
		else
			zerror "$(basename $f) is not installed"
		fi
	}
# }}}

# Prompt {{{
	# CVS integration
	setopt prompt_subst
	zstyle ':vcs_info:*' enable git svn
	zstyle ':vcs_info:*' get-revision true
	zstyle ':vcs_info:*' check-for-changes true
	zstyle ':vcs_info:*' unstagedstr "%F{red}"

	# Boldings break since %b is a special escape here. Bold is set
	# during print
	zstyle ':vcs_info:*' formats "%F{black}├─[%F{yellow}%s%F{black}:%F{magenta}%r%F{black}]-[%F{green}%u%b%F{black}]"
	zstyle ':vcs_info:(svn):*' branchformat '%b'

	p() {
		if [[ $1 =~ "true" ]] || [[ $1 =~ "false" ]] ; then
			export PKEEP=$1
		else
			export PMODE=$1
		fi
	}
	chpwd() {
		if $HASTODO && [ -f $TODOFILE ] ; then
			$TODO && echo
		fi

		if [ -n "$CHPWD" ] ; then
			ls
		fi
	}
	precmd () {
		if [ $TERM != "linux" ] ; then
			print -Pn "\e]0;%n@%m: %~\a"

			# Add svn?
			if [[ $PWD =~ "git/" ]] || [ -d "$PWD/.git" ] ; then
				if ! $PKEEP && [ $PMODE != 3 ] ; then
					export POLD=$PMODE
					export PMODE=3
				fi
			fi
		fi

		if [ $PMODE = 3 ] ; then
			vcs_info

			# If the message is empty, we are no longer in the CVS.
			if [ -z "${vcs_info_msg_0_}" ] ; then
				export PMODE=$POLD
			fi
		fi
		prompt
	}

	# Set prompt
	prompt() {
		if [ $PMODE -ge 2 ] ; then
			if [ $USER != $ALIAS ] ; then
				local u="%n@%m"
			else
				local u="%m"
			fi
			if $HASMULTI && [ $TERM != $MULTITERM ] ; then
				local dc=red
			else
				local dc=blue
			fi

			if [ -d $MAIL ] ; then
				local mc=$(find $MAIL | grep new/ | wc -l)
				if [ $mc -gt 0 ] ; then
					if [ $TCOLORS = 256 ] ; then
						local c=202
					else
						local c=red
					fi
					local m="─[%F{$c}${mc}M%F{black}]" # Mail
				fi
			fi

			local b=''

			# Kernel battery info, with primitive caching! \o/
			if $LAPTOP && [ -f $BAT ] ; then
				if [ -f $BATC ] && [ $(date +'%s') -le $BATT ]; then
					local bat=$(cat $BATC)
				else
					local bat=$(bat)
					echo $bat > $BATC

					# 60 second cache
					export BATT=$(($(date +'%s') + $BATS))
				fi
				local p=$(echo $bat | cut -f1 -d\|)
				local t=$(echo $bat | cut -f2 -d\|)

				local b="─[%F{yellow}$p%%%F{black}]─[%F{magenta}$t%F{black}]" # Battery
			fi

			local e='%(?..─[%F{red}$(bell)%?%F{black}])' # Errorcodes
			local j='%(1j.─[%F{yellow}%j%F{black}].)' # Jobs

			local r1="%B%F{black}┌─[%(#.%F{red}%m.%F{green}$u)%F{black}]─[%F{$dc}%~%F{black}]%b"
			local r2="%B%F{black}└${e}${b}${j}${m}─[%F{cyan}%D{%H:%M}%F{black}]>%f%b "
		fi

		case $PMODE in
			0) ; PROMPT="%# "; ;;
			1) ; PROMPT="%B%(#.%F{1}%m.%F{2}%n@%m) %F{4}%~ %#%b%f " ; ;;
			2) ; PROMPT=$(print "$r1\n$r2") ; ;;
			3) ; PROMPT=$(print "$r1\n%B${vcs_info_msg_0_}%b\n$r2")
		esac
	}
# }}}
# vim: ft=zsh fmr={{{,}}}
