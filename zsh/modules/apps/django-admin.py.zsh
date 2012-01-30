local psup=$XDG_DATA_HOME/python/djangoloader.py
if [[ -f $psup ]]; then
    export PYTHONSTARTUP=$psup
fi

export DBTMP="tmp"
export DBHOST="dt"
export REMOTEDB="mancx_django"
export LOCALDB="mancx_django"

alias dm='python2 manage.py'
alias ds='echo "no" | dm syncdb'
alias dz='ipython'
alias mm='django-admin.py makemessages -l en && django-admin.py compilemessages'

function dt() {
    coverage run manage.py test $*
}

_dtcomplete() {
    reply=()

    for f in ./apps/*(/); do
        reply+=(${f##*/})
    done
}

# Completion \o/
compctl -Y "%B%F{${c[24]}}app%f%b" -K _dtcomplete dt

function dsh() {
    mysql -e "drop database $LOCALDB ; create database $LOCALDB character set utf8 collate utf8_general_ci;"
    ds
}

function dsr() {
    if [[ ! -f "$PWD/manage.py" ]]; then
        _zerror "No django manager found. Exiting"
        return 1
    fi

    DATE=$(date +'%Y.%m.%d')
    F="$DBTMP/$DBHOST.$DATE.mysql"

    # Ignore laser tables and the large session table
    IGNORE=()

    i=""
    for x in $IGNORE ; do
        i+="--ignore-table=$REMOTEDB.$x "
    done

    if [[ ! -f $F ]] || [[ -n "$1" ]]; then
        echo "Grabbing $REMOTEDB from $DBHOST"
        ssh $DBHOST mysqldump $REMOTEDB $i > $F
    fi

    echo "Loading $F into $LOCALDB"
    mysql $LOCALDB < $F
}

function dr() {
    if [[ ! -f "manage.py" ]] ; then
        _zerror "No django manager found. Exiting"
        return 1
    fi

    rmext pyc &> /dev/null
    echo -e "\nRemoved .pyc files"

    if _has gunicorn_django; then
        gunicorn_django --workers=3 --pid=/tmp/gunicorn.pid
    else
        dm runserver 0.0.0.0:8000
    fi
}
