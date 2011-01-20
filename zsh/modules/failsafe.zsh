function fail()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Prints really angry text when the user misspells a command."
		fi
		return
	fi

	if _has figlet && [[ $PMODE != 0 ]] ; then
		# If figlet exists and we are not on a mobile connection, doit
		figlet :@ && echo
	else
		# Print red angry text
		echo -e "\n\033[1;31m   YOU'RE DOING IT WRONG! :@\033[0m\n"
	fi
}

alias sl='fail && ls'
alias ös='fail && ls'
alias lös='fail && ls'
alias xs='fail && cd'
alias vf='fail && cd'
alias cd..='fail && cd ..'
alias vom='fail && vim'
alias bim='fail && vim'
alias vum='fail && vim'
alias shitdown='fail && shutdown' # This has actually happened. Hilarity ensued.
