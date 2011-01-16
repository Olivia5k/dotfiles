export PYTHONSTARTUP=$XDG_DATA_HOME/python/djangoloader.py

alias dm='python2 manage.py'
alias dr='dm runserver'
alias ds='echo "no" | dm syncdb'
alias dz='bpython'

# Django sync hard. Useful when you update a model and the regular sync
# cant catch it. Note that extensive fixtures are crucial for this to be
# useful.
function dsh()
{
	for db in `mysql -s -s -e "show tables in dev_main"` ; do
		mysql -e "drop table dev_main.$db"
	done

	ds
}
