#!/bin/zsh
# git repo config file installer
# Written by Lowe Thiderman (daethorian@ninjaloot.se)
# WTFPL.

loc=$0:a:h
backup="$loc/old-configs-backup"

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

_link() {
    src=$1
    dst=$2
    nobackup=false

    if [[ -n "$3" ]]; then
        nobackup=true
    fi

    if [[ ! -L "$dst" ]] ; then
        if [[ -f $dst ]] || [[ -d $dst ]] ; then
            if $nobackup; then
                print "is local link. doing nothing."
                return
            fi
            if [[ ! -d $backup ]] ; then
                mkdir -p $backup
            fi
            mv $dst $backup
            print -P "Backed up original %B%F{10}${dst}%f%b"
        fi

        mkdir -p $dst:h &> /dev/null
        ln -s $src:a $dst:a
        print -P "%B%F{10}${src#./}%f%b installed to %B%F{12}${dst}%b"
    else
        print -P "%B%F{9}${src#./}%f%b is already installed"
    fi
}

_install() {
    while [[ -n "$1" ]]; do
        src=$1

        dst=$dest[$src]
        if [[ -z "$dst" ]]; then
            dst="$HOME/.${${(s:/:)src}[-1]}"
        fi

        _link $src $dst
        shift
    done
}

# lol print help
if [[ -z "$1" ]] || [[ "$1" =~ "-?-h(elp)?" ]]; then
    echo "install.sh:"
    echo "hax0r script helper for managing github config files"
    echo "Installation of configurations will be made by symbolic linking."
    echo
    echo "Any arguments given will be considered as configs for installation."
    echo "If none are given, all configurations are installed"
    echo "Possible choices are:"
    echo ${(ko)apps}
    echo
    echo "Possible options are:"
    echo '-a'
    echo "Installs all configurations."
    echo
    echo '-l <repo> [<args>]'
    echo "Sets up local monkeypatch repo."
    echo "Follows normal argument procedure afterwards."
    echo
    echo '-u'
    echo "Does a git submodule update on all submodules."
    echo
    echo '-a or --all'
    echo "Prints this crude help message."
    exit 0
fi

# git initialization
#git submodule init
#git submodule update
#git submodule foreach git submodule init
#git submodule foreach git submodule update

if [[ "$1" = "-u" ]]; then
    print "Git updating done"
    exit 0
fi

if [[ "$1" = "-l" ]]; then
    if [[ -z "$2" ]]; then
        print "Fatal error: Gief monkeypatch repo addrz plx"
        exit 1
    fi
    git clone $2 ./local || exit 1

    for f in local/**/*(.); do
        dst=${f#local/}
        ins=false

        if [[ ! -f $loc/$dst ]]; then
            dst=$dst:h

            # Check if the directory the file wants to live in exists
            if [[ -d "$dst" ]]; then
                _link $f $dst true
                ins=true
                continue
            fi

            f=$f:h
            # Traverse upwards
            while [[ "$dst" =~ "/" ]]; do
                if [[ ! -d "$dst" ]]; then
                    _link $f $dst true
                    ins=true
                    break
                fi
                f=$f:h
                dst=$dst:h
            done
        fi

        if ! $ins; then
            print -P "%B%F{9}${f#./}%f%b not installed: already exists"
        fi
    done

    print
    shift 2
fi

if [[ -z "$1" ]] || [[ "$1" = "-a" ]]; then
    for a in $apps; do
        _install ${(z)a}
    done
else
    while [[ -n "$1" ]]; do
        _install ${(z)${apps[$1]}}
        shift
    done
fi

# These directories are required
mkdir -p $HOME/.cache/vim/{backup,tmp} $HOME/.logs $HOME/.local/{bin,share} &> /dev/null
