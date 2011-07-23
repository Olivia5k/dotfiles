#!/bin/zsh
# git repo config file installer
# Written by Lowe Thiderman (daethorian@ninjaloot.se)
# WTFPL.

typeset -A apps
typeset -A dest

a=('./vim' './vim/vimrc')
apps[vim]=$a

a=('./misc/x11/Xdefaults' './misc/x11/xinitrc')
apps[x11]=$a

a=('./zsh/zshrc')
apps[zsh]=$a

a=('./tmux/tmux.conf')
apps[tmux]=$a

a=('./uzbl/')
apps[uzbl]=$a
dest[./uzbl/]="$XDG_CONFIG_HOME/uzbl"

a=('./misc/mplayer')
apps[mplayer]=$a

a=('./misc/mostrc')
apps[most]=$a

a=('./misc/terminfo')
apps[terminfo]=$a

a=('./misc/ncmpcpp.conf')
apps[ncmpcpp]=$a
dest[./misc/ncmpcpp.conf]="$HOME/.ncmpcpp/config"

a=('./misc/i3.conf')
apps[i3]=$a
dest[./misc/i3.conf]="$HOME/.i3/config"

_ins() {
    backup="$PWD/old-configs-backup"
    while [[ -n "$1" ]]; do
        src=$1

        dst=$dest[$src]
        if [[ -z "$dst" ]]; then
            dst="$HOME/.${${(s:/:)src}[-1]}"
        fi

        if [[ ! -L "$dst" ]] ; then
            if [[ -f $dst ]] || [[ -d $dst ]] ; then
                if [[ ! -d $backup ]] ; then
                    mkdir -p $backup
                fi
                mv $dst $backup
                print -P "Backed up original %B%F{10}${dst}%f%b"
            fi


            mkdir -p $(dirname $dst) &> /dev/null
            ln -s $(readlink -f $src) $dst
            print -P "%B%F{10}${src#./}%f%b installed to %B%F{12}${dst}%b"
        else
            print -P "%B%F{9}${src#./}%f%b is already installed"
        fi
        shift
    done
}

if [ "$PWD" = "$HOME/config" ] ; then
    if [[ -z "$1" ]]; then
        for a in $apps; do
            _ins ${(z)a}
        done
    else
        while [[ -n "$1" ]]; do
            _ins ${(z)${apps[$1]}}
            shift
        done
    fi
    mkdir -p $HOME/.cache/vim/{backup,tmp} $HOME/.logs $HOME/.local/{bin,share} &> /dev/null

    git submodule init
    git submodule update
    git submodule foreach git submodule init
    git submodule foreach git submodule update
else
    print -Pn "The configs should be placed inside %B%F{12}~/config%f%b, "
    print "and the script should be run from there."
    exit 1
fi
