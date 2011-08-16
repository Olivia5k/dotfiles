export PYTHONSTARTUP=$XDG_DATA_HOME/python/djangoloader.py

alias dm='python2 manage.py'
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

    mysql -e "drop database dev_main ; create database dev_main character set utf8 collate utf8_general_ci;"
    ds
}

function dr()
{
    if [[ ! -f "manage.py" ]] ; then
        _zerror "No django manager found. Exiting"
        return 1
    fi

    rmext pyc
    dm runserver 0.0.0.0:8000
}

function dcompile()
{
    if [[ ! -f "manage.py" ]] ; then
        _zerror "No django manager found. Exiting"
        return 1
    fi

    root=$PWD

    for d in **/locale ; do
        cd ${d%locale} &> /dev/null

        if [[ -z "$1" ]]; then
            for dd in locale/*(/); do
                django-admin.py makemessages -l ${dd#locale/}
            done
        fi

        django-admin.py compilemessages
        cd $root &> /dev/null
    done
}
