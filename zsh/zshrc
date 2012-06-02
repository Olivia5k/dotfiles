# zshrc by Lowe Thiderman (daethorian@ninjaloot.se)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).
# https://github.com/daethorian/conf-zsh
#
# TODO:
# Fix the completion-adds-letter bug

# zsh configuration directory; dynamically found
export ZSHRC="$HOME/.zshrc"
export ZSHCONFDIR=$ZSHRC:A:h
export CONFIG=$ZSHCONFDIR:h

# The XDG standard is indeed quite exquisite.
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_DIRS=${XDG_CONFIG_DIRS:-/etc/xdg}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/share/:/usr/local/share/}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}

export TCOLORS=$(echotc Co)

if [[ "$UID" != 0 ]]; then
    fpath=( $ZSHCONFDIR/completion "${fpath[@]}" )
fi

autoload -Uz compinit
compinit -d $XDG_DATA_HOME/zsh/compdump

autoload colors zsh/terminfo  # Colors
autoload -Uz vcs_info  # git integration
bindkey -e

function zerror() {
    print -P "%B%F{${c[6]}}Error%f%b:" $*
}
function has() {
    [[ -x $commands[$1] ]] && return true # zsh style \o/
}
function _modload() {
    local m
    local f
    for m in $* ; do
        # If a path is given as absolute (the cores are), just load it.
        if [[ "$m" =~ "^/" ]] ; then
            f=$m
        else
            f="$ZMODDIR/${m}.zsh"
        fi

        if [[ -f "$f" ]] ; then
            source $f
        else
            zerror "$m is not a valid module."
        fi
    done
}

# LS_COLORS. Placed in master as part of the main conf.
lscf=$ZSHCONFDIR/modules/ext/LS_COLORS/LS_COLORS
if [[ -f $lscf ]] && [[ $TCOLORS = 256 ]]; then
    eval $(dircolors -b $lscf)
fi

local USERFILE="$ZSHCONFDIR/local.zsh"
if [[ -f $USERFILE ]] || [[ -L $USERFILE ]] ; then
    source $USERFILE
else
    source $ZSHCONFDIR/daethorian.zsh
fi

# Kill root after $ROOT_TIMEOUT seconds
if [[ "$UID" = 0 ]] && [[ -n "$ROOT_TIMEOUT" ]] ; then
    print -P "Warning: Root shell will timeout after %B${c[12]}$ROOT_TIMEOUT seconds%f%b."
    TMOUT=$ROOT_TIMEOUT
fi

# zsh-sysadmin. Quickfast!
fdb=$ZSHCONFDIR/modules/ext/zsh-filedb/zsys.zsh
if [[ -f $fdb ]]; then
    source $fdb
fi

for d in $HOMEBIN $LOGS $ZDUMPDIR ; do
    if [[ ! -d $d ]] ; then
        _zdebug "Autocreating %B%F{${c[4]}}${d}%b%f"
        mkdir -p $d &> /dev/null
    fi
done

# vim: ft=zsh
