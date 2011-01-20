# CVS integration
setopt prompt_subst
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr "%F{red}" # Just make it red!

repo="%B%F{black}├─[%F{yellow}%s%F{black}:%F{magenta}%r%F{black}]"
branch="─[%F{green}%u%b%F{black}]%%b"
zstyle ':vcs_info:*' formats "${repo}${branch}"
zstyle ':vcs_info:*' actionformats "${repo}─[%F{cyan}%a%F{black}]${branch}"
zstyle ':vcs_info:(svn):*' branchformat '%b'

function p()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Change prompt behaviour."
		fi
		return
	fi

	if [[ "$1" = "3" ]] ; then
		export PKEEP=false
	else
		export PKEEP=true
	fi

	export PMODE=$1
}

# Since the two functions below are potentially executed on every prompt
# reload, they do not have any inboud documentation.

function precmd()
{
	if [[ $TERM != "linux" ]] ; then
		# Print xterm title
		print -Pn "\e]0;%n@%m: %~\a"

		# Add svn?
		if [[ $PWD =~ "git/" ]] || [[ -d "$PWD/.git" ]] ; then
			if ! $PKEEP && [[ $PMODE != 3 ]] ; then
				export POLD=$PMODE
				export PMODE=3
			fi
		fi
	fi

	if [[ $PMODE = 3 ]] ; then
		vcs_info

		# If the message is empty, we are no longer in the CVS.
		if [[ -z "${vcs_info_msg_0_}" ]] ; then
			export PMODE=$POLD
		fi
	fi
	prompt
}

function prompt()
{
	if [[ $PMODE -ge 2 ]] ; then
		if [[ "$USER" != "$ALIAS" ]] ; then
			local u="%n@%m"
		else
			local u="%m"
		fi
		if $HASMULTI && [[ $TERM != $MULTITERM ]] ; then
			local dc=red
		else
			local dc=blue
		fi

		if [[ -d $MAIL ]] ; then
			local mc=$(find $MAIL | grep new/ | wc -l)
			if [[ $mc -gt 0 ]] ; then
				if [[ $TCOLORS = 256 ]] ; then
					local c=202
				else
					local c=red
				fi
				local m="─[%F{${c}}${mc}M%F{black}]" # Mail
			fi
		fi

		local t=""
		if [[ -n "$TMOUT" ]] && [[ $TMOUT != 0 ]] ; then
			local t="─[%F{red}${TMOUT}%F{black}]"
		fi

		local b=''
		# Kernel battery info, with primitive caching! \o/
		if $LAPTOP && [[ -f $BAT ]] ; then
			if [[ -f $BATC ]] && [[ $(date +'%s') -le $BATT ]]; then
				local bat=$(cat $BATC)
			else
				local bat=$(bat)
				echo $bat > $BATC

				# 60 second cache
				export BATT=$(($(date +'%s') + $BATS))
			fi
			local p=$(echo $bat | cut -f1 -d\|)
			local t=$(echo $bat | cut -f2 -d\|)

			local b="─[%F{yellow}${p}%%%F{black}]─[%F{magenta}${t}%F{black}]" # Battery
		fi

		local e='%(?..─[%F{red}$(bell)%?%F{black}])' # Errorcodes
		local j='%(1j.─[%F{yellow}%j%F{black}].)' # Jobs

		local r1="%B%F{black}┌─[%(#.%F{red}%m.%F{green}$u)%F{black}]$t─[%F{$dc}%~%F{black}]%b"
		local r2="%B%F{black}└${e}${b}${j}${m}─[%F{cyan}%D{%H:%M}%F{black}]>%f%b "
	fi

	case $PMODE in
		0) ; PROMPT="%# "; ;;
		1) ; PROMPT="%B%(#.%F{1}%m.%F{2}%n@%m) %F{4}%~ %#%b%f " ; ;;
		2) ; PROMPT=$(print "$r1\n$r2") ; ;;
		3) ; PROMPT=$(print "$r1\n${vcs_info_msg_0_}\n$r2")
	esac
}
