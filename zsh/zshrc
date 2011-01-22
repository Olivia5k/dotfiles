# zshrc by Lowe Thiderman (daethorian@ninjaloot.se)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).
#
# <github link>

# This zsh configuration is built to be incredibly modular, useful and usable by
# others than myself. It keeps the core stuff that (probably) suits everyone
# pretty tight but leaves alot of options and customizability. Other users can
# modify their own parts (the $USER.zsh file) and do whatever they want without
# ever creating conflicts when updating the git repo.

# Also notable is that alot of aliases and modules are included and are
# application specific. None of them will be loaded if the applications are not
# found.

# Compiled with bits and pieces from all over the place.  Notable sources are the
# zsh manpages, zsh lovers, Phil!'s prompt and the Arch Linux BBS.

# http://grml.org/zsh/zsh-lovers.html
# http://aperiodic.net/phil/prompt/
# https://bbs.archlinux.org
#
# TODO:
# Add colorschemes \o/
# Make dirdiff more useful
# Wallpaper management (check for $DISPLAY)
# Extend grab() to shift arguments (-g)
# Add mplayer aliases

# Colors. You are expected to be wanting those.
autoload colors zsh/terminfo

# git and svn integration
autoload -Uz vcs_info

# zshs awesome renamer
autoload zmv

# URL escaping. Whenever you paste a URL to your terminal, zsh will escape
# any characters with special meaning to it.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

function _zdebug()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Internal debug message printer"
		fi
		return
	fi

	if [[ -n "$DEBUG" ]] && $DEBUG ; then
		print -P "%B%F{${c[18]}}Debug%f%b:" $*
	fi
}

function _zerror()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Internal error message printer"
		fi
		return
	fi

	print -P "%B%F{${c[6]}}Error%f%b:" $*
}

function _has()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Internal checker if an application exists."
		fi
		return
	fi

	which $1 &> /dev/null ; return $?
}

# zsh configuration directory; dynamically found
export ZSHCONFDIR=$(dirname $(readlink $HOME/.zshrc))

# Colorscheme. Load default as fallback
source $ZSHCONFDIR/colorschemes/default.zsh

# File with variables that most probably changes per user.
# Most documentation about this configuration is in there.
# If no file is found, using daethorians default.
local USERFILE="$ZSHCONFDIR/$USER.zsh"
if [[ -f $USERFILE ]] ; then
	source $USERFILE
else
	source $ZSHCONFDIR/daethorian.zsh
fi

# Prompt variables
# To avoid checking for these every time the prompt is rendered, they are
# stored in global variables.
export TCOLORS=$(echotc Co)

if [[ -n "$MULTI" ]] && _has $MULTI; then
	export HASMULTI=true
else
	export HASMULTI=false
fi
if [[ -n "$TODO" ]] && _has $TODO ; then
	export HASTODO=true
else
	export HASTODO=false
fi

if [[ $TERM = "linux" ]] && $FORCE_CONSOLE; then
	export PMODE=1
elif [[ $TERM = "xterm" ]] && $FORCE_MOBILE ; then
	export PMODE=0
fi

# Store last prompt; for use with CVS prompt
export POLD=$PMODE

# Kill root after three minutes
if [[ "$UID" = 0 ]] && [[ -n "$ROOT_TIMEOUT" ]] ; then
	print -P "Warning: Root shell will timeout after %B%F{${c[12]}}$ROOT_TIMEOUT seconds%f%b."
	TMOUT=$ROOT_TIMEOUT
fi

for d in $HOMEBIN $LOGS $ZDUMPDIR ; do
	if ! [[ -d $d ]] ; then
		_zdebug "Autocreating %B%F{${c[4]}}${d}%b%f"
		mkdir -p $d &> /dev/null
	fi
done

# vim: ft=zsh
