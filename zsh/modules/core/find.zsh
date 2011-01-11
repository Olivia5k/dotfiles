# Find a file.
function f()
{
	find 2>/dev/null | grep -is "$1"
}

# Todo grep. Prints reminding lines that developers leave in their code.
# (gt is taken by git stash)
function tg()
{
	grep '(TODO|XXX|FIXME)' * -ERIin
}

# Global grep. Searches files for content.
function gg()
{
	grep "$*" * -RIins
}
# Global grep extended. Searches files for content with real regular expressions.
function gge()
{
	grep "$*" * -ERIins
}

# Same as above, but does not print the matching line. Useful when searching
# through files with criminally long lines.
function ggl()
{
	grep "$*" * -RlIis
}

# Same as above, but instead of printing the files, open them in your editor
function eggl()
{
	$EDITOR $(ggl $*)
}

# List differential files between two directories
function dirdiff()
{
	if [ -d $1 ] && [ -d $1 ] ; then
		_dirdiff $1 $2
		_dirdiff $2 $1
	else
		zerror "Both arguments must be valid directories"
	fi
}

function _dirdiff()
{
	local listfile="/tmp/.zshlistfile"
	find $2 -type f > $listfile

	print -P "\n%BFiles in %F{blue}${1}%f not in %F{blue}${2}%f%b:"
	for i in $(find $1 -type f) ; do
		f=$(basename $i)
		if ! grep $f $listfile &> /dev/null; then
			echo $i
		fi
	done
	rm $listfile
}
