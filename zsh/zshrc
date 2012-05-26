# zshrc by Lowe Thiderman (daethorian@ninjaloot.se)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).
# https://github.com/daethorian/conf-zsh
#
# TODO:
# Fix the completion-adds-letter bug

fpath=( $ZSHCONFDIR/completion "${fpath[@]}" )
autoload -Uz compinit; compinit
autoload colors zsh/terminfo  # Colors. You are expected to be wanting those.
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

# zsh configuration directory; dynamically found
export ZSHCONFDIR=$0:A:h
export CONFIF=$ZSHCONFDIR:h

# The most useful alias there ever was
alias zz="source ~/.zshrc"

export TCOLORS=$(echotc Co)

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
    print -P "Warning: Root shell will timeout after %B%F{${c[12]}}$ROOT_TIMEOUT seconds%f%b."
    TMOUT=$ROOT_TIMEOUT
fi

# zsh-filedb. Quickfast!
fdb=$ZSHCONFDIR/modules/ext/zsh-filedb/filedb.zsh
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
