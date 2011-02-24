export PYTHONSTARTUP=$XDG_DATA_HOME/python/djangoloader.py

alias dm='python2 manage.py'
alias dr='dm runserver'
alias ds='echo "no" | dm syncdb'
alias dz='bpython'

# Django sync hard. Useful when you update a model and the regular sync
# cant catch it. Note that extensive fixtures are crucial for this to be
# useful.
function dsh()##
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Resync a Django database"
		elif [[ "$2" =~ "l(ong)?" ]] ; then
			local str="Drops all mysql database tables inside ´dev_main´\n"
			str+="and reloads all fixtures. Useful when editing existing\n"
			str+="models and the regular sync won't catch it.\n\n"
			str+="Note that without fixtures, this just kills all your data."
			echo $str
		fi
		return
	fi

	#for db in `mysql -s -s -e "show tables in dev_main"` ; do
		#mysql -e "drop table dev_main.$db"
	#done

	mysql -e "drop database dev_main ; create database dev_main;"
	ds
}
