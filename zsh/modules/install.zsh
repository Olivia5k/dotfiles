# Install an executable into $HOMEBIN
function ins()
{
	f=$(readlink -f $1)
	if [ -f $f ] && [ -x $f ] ; then
		if [ -n "$2" ] ; then
			local target=$2
		else
			local target=$(basename $1)
		fi

		if ! has $target ; then
			print -P "Installing %B%F{green}$(basename $1)%f%b as %B%F{green}${target}%f%b"
			ln -s $f $HOMEBIN/$target
		else
			zerror "$target is already installed"
		fi
	else
		zerror "$(basename $f) is not an executable file"
	fi
}

# Remove an executable installed by ins()
function unins()
{
	f=$HOMEBIN/$(basename $1)
	if [ -L $f ] ; then
		print -P "Uninstalling %B%F{green}$(basename $1)%f%b."
		unlink $f
	else
		zerror "$(basename $f) is not installed"
	fi
}
