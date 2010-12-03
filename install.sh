#!/bin/bash
# git repo config file installer
# Written by Daethorian (daethorian@ninjaloot.se)
# WTFPL.

# TODO: Remake as python script with real options and uninstallation.

if [ "$PWD" = "$HOME/config" ] ; then
	backup="$PWD/.bak"

	if [ -n "$1" ] ; then
		pretend=true
	else
		pretend=false
	fi

	_ins() {
		src=$1
		dest=$2
		if [ ! -L $dest ] ; then
			if ! $pretend ; then
				if [ -f $dest ] || [ -d $dest ] ; then
					if [ ! -d $backup ] ; then
						mkdir -p $backup
					fi
					mv $dest $backup
				fi

				ln -s $(readlink -f $src) $dest
				echo "$src installed"
			else
				echo "$src would have been installed"
			fi
		fi
	}

	dirs[1]='mplayer'
	dirs[2]='mutt'
	dirs[3]='ncmpcpp'
	dirs[4]='vim'
	dirs[5]='weechat'
	dirs[6]='terminfo'

	for i in `seq 1 ${#dirs[@]}` ; do
		_ins "./${dirs[$i]}" "$HOME/.${dirs[$i]}"
	done

	files[1]="most/mostrc"
	files[2]="stumpwm/stumpwmrc"
	files[3]="tmux/tmux.conf"
	files[4]="vim/vimrc"
	files[5]="x/Xdefaults"
	files[6]="x/xinitrc"
	files[7]="zsh/zshrc"

	for i in `seq 1 ${#files[@]}` ; do
		_ins "./${files[$i]}" "$HOME/.$(basename ${files[$i]})"
	done

	# Create directories that these configs assume exists
	mkdir -p $HOME/.cache/vim/{backup,tmp} $HOME/.logs $HOME/.local/{bin,share} &> /dev/null
else
	echo "The configs should be placed inside ~/config, and the script should be run from there."
	exit 1
fi
