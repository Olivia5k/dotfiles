function chpwd()
{
	if $HASTODO && [[ -f $TODOFILE ]] ; then
		$TODO && echo
	fi

	ls
}
