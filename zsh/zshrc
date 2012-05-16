# zshrc by Lowe Thiderman (daethorian@ninjaloot.se)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).
#
# <github link>

# This zsh configuration is built to be incredibly modular, useful and usable
# by others than myself. It keeps the core stuff that (probably) suits everyone
# pretty tight but leaves alot of options and customizability. Other users can
# modify their own parts (the $USER.zsh file) and do whatever they want without
# ever creating conflicts when updating the git repo.

# Also notable is that alot of aliases and modules are included and are
# application specific. None of them will be loaded if the applications are not
# found.

# Compiled with bits and pieces from all over the place.  Notable sources are
# the zsh manpages, zsh lovers, Phil!'s prompt and the Arch Linux BBS.

# http://grml.org/zsh/zsh-lovers.html
# http://aperiodic.net/phil/prompt/
# https://bbs.archlinux.org
#
# TODO:
# Make dirdiff more useful
# Wallpaper management (check for $DISPLAY)
# Extend grab() to shift arguments (-g)
# Add mplayer aliases
# Add bindkey modules
#    Add exitstatus resetter
# Fix the completion-adds-letter bug

fpath=( $ZSHCONFDIR/completion "${fpath[@]}" )
autoload -Uz compinit
compinit

# Colors. You are expected to be wanting those.
autoload colors zsh/terminfo

# git and svn integration
autoload -Uz vcs_info

# zshs awesome renamer
autoload zmv

# lol keys
bindkey -e

function _zdebug() {
    if [[ -n "$ZDEBUG" ]]; then
        print -P "%B%F{${c[18]}}Debug%f%b:" $*
    fi
}

function _zerror() {
    print -P "%B%F{${c[6]}}Error%f%b:" $*
}

function _has() {
    [[ -x $commands[$1] ]] && return true # zsh style \o/
}

function _modload() {
    local m
    for m in $* ; do
        # If a path is given as absolute (the cores are), just load it.
        if [[ "$m" =~ "^/" ]] ; then
            f=$m
        else
            f="$ZMODDIR/${m}.zsh"
        fi

        if [[ -f "$f" ]] ; then
            _zdebug "Loading $m"
            source $f
            ZMODULES+=(${f##*/})
        else
            _zerror "$m is not a valid module."
        fi
    done
}

# zsh configuration directory; dynamically found
export ZSHCONFDIR=$(dirname $(readlink $HOME/.zshrc))

# The most useful alias there ever was
alias zz="source ~/.zshrc"

# Prompt variables
# To avoid checking for these every time the prompt is rendered, they are
# stored in global variables.
export TCOLORS=$(echotc Co)

# File with variables that most probably changes per user.
# Most documentation about this configuration is in there.
# If no file is found, using daethorians default.
local USERFILE="$ZSHCONFDIR/local.zsh"
if [[ -f $USERFILE ]] || [[ -L $USERFILE ]] ; then
    source $USERFILE
else
    source $ZSHCONFDIR/daethorian.zsh
fi

# Colorscheme. Load default as fallback
cf=$ZSHCONFDIR/colorschemes/$ZCOLOR.zsh
if [[ -f "$cf" ]] ; then
    source $cf
fi

HASMULTI=$([[ -n "$MULTI" ]] && _has $MULTI)

if [[ $TERM = "linux" ]] && $FORCE_CONSOLE; then
    export PMODE=1
elif [[ $TERM = "xterm" ]] && $FORCE_MOBILE ; then
    export PMODE=0
fi

if [ -f "/usr/lib/stderred.so" ]; then
    export LD_PRELOAD="/usr/lib/stderred.so"
fi

# Store last prompt; for use with CVS prompt
export POLD=$PMODE

# Kill root after three minutes
if [[ "$UID" = 0 ]] && [[ -n "$ROOT_TIMEOUT" ]] ; then
    print -P "Warning: Root shell will timeout after %B%F{${c[12]}}$ROOT_TIMEOUT seconds%f%b."
    TMOUT=$ROOT_TIMEOUT
fi

# LS_COLORS. Placed in master as part of the main conf.
lscf=$ZSHCONFDIR/modules/ext/LS_COLORS/LS_COLORS
if [[ -f $lscf ]] && [[ $TCOLORS = 256 ]]; then
    eval $(dircolors -b $lscf)
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
