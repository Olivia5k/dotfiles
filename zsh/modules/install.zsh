function ins()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Install an executable into \$HOMEBIN"
		fi
		return
	fi

	f=$(readlink -f $1)
	if [[ -f $f ]] && [[ -x $f ]] ; then
		if [[ -n "$2" ]] ; then
			local target=$2
		else
			local target=$(basename $1)
		fi

		if ! _has $target ; then
			print -P "Installing %B%F{${c[19]}}$(basename $1)%f%b as %B%F{${c[19]}}${target}%f%b"
			ln -s $f $HOMEBIN/$target
		else
			_zerror "$target is already installed"
		fi
	else
		_zerror "$(basename $f) is not an executable file"
	fi
}

function unins()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Remove an executable installed by ins()"
		fi
		return
	fi
	f=$HOMEBIN/$(basename $1)
	if [[ -L $f ]] ; then
		print -P "Uninstalling %B%F{${c[19]}}$(basename $1)%f%b."
		unlink $f
	else
		_zerror "$(basename $f) is not installed"
	fi
}
