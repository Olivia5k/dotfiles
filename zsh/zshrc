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
# Add docstring style to all functions and add a function that prints them
# Add prompt colorschemes \o/
# Make PKEEP more logical
# Make dirdiff more useful
# Add daemon completion and listing
# Actually, add general completion for the functions

# Add git configurator
# Add git rebase prompt
# Add git graph aliases

# Core {{{
	# Colors. You are expected to be wanting those.
	autoload colors zsh/terminfo

	# git and svn integration
	autoload -Uz vcs_info

	# URL escaping. Whenever you paste a URL to your terminal, zsh will escape
	# any characters with special meaning to it.
	autoload -Uz url-quote-magic # URL escaping
	zle -N self-insert url-quote-magic

	function zdebug() {
		if [ -n "$DEBUG" ] && $DEBUG ; then
			print -P "%B%F{cyan}Debug%f%b:" $*
		fi
	}
	function zerror() {
		print -P "%B%F{red}Error%f%b:" $*
	}

	# Helper function and setter
	# With this, one can just run if $(has <application>) ; then [...].
	function has() {
		which $1 &> /dev/null ; return $?
	}
# }}}
# Variables {{{
	# zsh configuration directory
	ZSHCONFDIR="$HOME/config/zsh"

	# File with variables that most probably changes per user.
	# Most documentation about this configuration is in there.
	# If no file is found, using daethorians default.
	local USERFILE="$ZSHCONFDIR/$USER.zsh"
	if [ -f $USERFILE ] ; then
		source $USERFILE
	else
		source $ZSHCONFDIR/daethorian.zsh
	fi

	# Prompt variables
	# To avoid checking for these every time the prompt is rendered, they are
	# stored in global variables.
	export TCOLORS=$(echotc Co)

	if [ -n "$MULTI" ] && has $MULTI; then
		export HASMULTI=true
	else
		export HASMULTI=false
	fi
	if [ -n "$TODO" ] && has $TODO ; then
		export HASTODO=true
	else
		export HASTODO=false
	fi

	if [ $TERM = "linux" ] && $FORCE_CONSOLE; then
		export PMODE=1
	elif [ $TERM = "xterm" ] && $FORCE_MOBILE ; then
		export PMODE=0
	fi

	# Store last prompt; for use with CVS prompt
	export POLD=$PMODE

	# Kill root after three minutes
	if [ "$UID" = 0 ] && [ -n "$ROOT_TIMEOUT" ] ; then
		print -P "Warning: Root shell will timeout after %B%F{red}$ROOT_TIMEOUT seconds%f%b."
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

# vim: ft=zsh fmr={{{,}}}
