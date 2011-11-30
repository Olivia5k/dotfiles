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

function _link() {
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

function _install() {
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
if [[ "$1" =~ "-?-h(elp)?" ]]; then
    echo "install.zsh:"
    echo "hax0r script helper for managing github config files"
    echo "Installation of configurations will be made by symbolic linking."
    echo
    echo "Any arguments given will be considered as configs for installation."
    echo "Possible choices are:"
    echo ${(ko)apps}
    echo
    echo "If no arguments are given, configurations will be installed if their"
    echo "appications are found, with the exception of terminfo, which will"
    echo "always be installed to make sure that users of cool terminals can"
    echo "always ssh to boxen with these configurations."
    echo
    echo "Possible options are:"
    echo '-a'
    echo "Installs all configurations."
    echo
    echo '-l <github username>'
    echo "Sets up local monkeypatch repo."
    echo "Follows detection installation afterwards."
    echo
    echo '-u'
    echo "Does a git submodule update on all submodules."
    echo
    echo '-h or --help'
    echo "Prints this crude help message."
    exit 0
fi

git submodule init
git submodule update
git submodule foreach git submodule init
git submodule foreach git submodule update

if [[ "$1" = "-u" ]]; then
    # This is actually it. Since the rows abowe do what they do, this step
    # really only needs to stop the script.
    print "Git updating done"
    exit 0
fi

if [[ "$1" = "-l" ]]; then
    if [[ -z "$2" ]]; then
        print "Fatal error: Gief monkeypatch repo addrz plx"
        exit 1
    fi

    git clone $2 ./local || exit 1

    for t in zsh vim; do
        if [[ -f ./local.$t ]]; then
            _link ./local.$t ./$t/
        fi
    done

    if [[ -f .local/gitconfig ]] ; then
        _link ./local/gitconfig $HOME/.gitconfig
    fi
fi

# Install those detected
if [[ -z "$1" ]] || [[ "$1" = "-l" ]]; then
    for a in ${(ko)apps}; do
        if [[ -x $commands[$a] ]] || [[ "$a" = "terminfo" ]] ; then
            _install ${(z)${apps[$a]}}
        fi
    done

# Install all of them
elif [[ "$1" = "-a" ]]; then
    for a in $apps; do
        _install ${(z)a}
    done

# Install specified
elif [[ -n "$1" ]] ; then
    while [[ -n "$1" ]]; do
        _install ${(z)${apps[$1]}}
        shift
    done
fi

if [[ -r $HOME/.gitconfig ]]; then
    name=$(git config user.name)
    email=$(git config user.email)
    print -P "%B%F{10}NOTE%f%b: git was installed for $name ($email)"
else
    print -P "%B%F{10}NOTE%f%b: No git configuration file was found."
fi

# These directories are required
mkdir -p $HOME/{.cache/vim/{backup,tmp},.logs,.local/{bin,share}} &> /dev/null
