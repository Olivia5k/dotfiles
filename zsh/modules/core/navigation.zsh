# These three are mostly useful when on mobile connections and things are slow.
function cl()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "cd && ls"
		fi
		return
	fi

	cd $1 && ls
}

function cll()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "cd && ls -l"
		fi
		return
	fi

	cd $1 && ls -l
}

function clal()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "cd && lal"
		fi
		return
	fi

	cd $1 && ls -al
}

alias '..'='cd ..'
alias '...'='cd ../..'
alias '....'='cd ../../..'
alias '.....'='cd ../../../..'
