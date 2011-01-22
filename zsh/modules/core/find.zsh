function f()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Find files based on their filenames"
		fi
		return
	fi

	find 2>/dev/null | grep -is "$1"
}

function fd()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Find directories based on their filenames"
		fi
		return
	fi

	find 2>/dev/null -type d | grep -is "$1"
}

# Todo grep. Prints reminding lines that developers leave in their code.
# (gt is taken by git stash)
function tg()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Grep for developer comments in source code"
		fi
		return
	fi

	grep '(TODO|XXX|FIXME)' * -ERIin
}

# Global grep. Searches files for content.
function gg()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Search files for content"
		fi
		return
	fi

	grep "$*" * -RIins
}
# Global grep extended. Searches files for content with real regular expressions.
function gge()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Search files for content, regexp style"
		fi
		return
	fi

	grep "$*" * -ERIins
}

# Same as above, but does not print the matching line. Useful when searching
# through files with criminally long lines.
function ggl()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Search files for content, regexp style. Prints matching file only."
		fi
		return
	fi

	grep "$*" * -RlIis
}

# Same as above, but instead of printing the files, open them in your editor
function eggl()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Searches files for content and opens matching files in EDITOR"
		fi
		return
	fi

	if [[ $EDITOR = $(which vim) ]] ; then
		$EDITOR -p $(ggl $*)
	else
		$EDITOR $(ggl $*)
	fi
}

# List differential files between two directories
function dirdiff()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "List differential files between two directories"
		elif [[ "$2" =~ "l(ong)?" ]] ; then
			echo "Long desc"
		fi
		return
	fi

	if [[ -d "$1" ]] && [[ -d "$1" ]] ; then
		_dirdiff $1 $2
		_dirdiff $2 $1
	else
		_zerror "Both arguments must be valid directories"
	fi
}

function _dirdiff()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Internal helper function to dirdiff()"
		fi
		return
	fi

	local listfile="/tmp/.zshlistfile"
	find $2 -type f > $listfile

	print -P "\n%BFiles in %F{${c[4]}}${1}%f not in %F{${c[4]}}${2}%f%b:"
	for i in $(find $1 -type f) ; do
		f=$(basename $i)
		if ! grep $f $listfile &> /dev/null; then
			echo $i
		fi
	done
	rm $listfile
}
