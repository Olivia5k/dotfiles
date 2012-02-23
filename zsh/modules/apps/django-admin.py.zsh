local psup=$XDG_DATA_HOME/python/djangoloader.py
if [[ -f $psup ]]; then
    export PYTHONSTARTUP=$psup
fi

export DBTMP="tmp"
export DBHOST="dt"
export REMOTEDB="mancx_django"
export LOCALDB="mancx_django"
export CODE_ROOT="/srv/live"

alias dm='python2 manage.py'
alias ds='echo "no" | dm syncdb'
alias dz='ipython'

function mm() {
    if [[ -n "$1" ]]; then
        while [[ -n "$1" ]]; do
            _mm apps/$1/
            shift
        done
    else
        for dir in apps/*(/) ; do
            if [[ -d $dir/locale  ]]; then
                _mm $dir
            fi
        done
    fi
}

function _mm() {
    cd -q $1

    django-admin.py makemessages -l en
    django-admin.py compilemessages

    cd -q ../..
}

function dt() {
    coverage run manage.py test $*
}

_appscomplete() {
    reply=()

    for f in ./apps/*(/); do
        reply+=(${f##*/})
    done
}

# Completion \o/
compctl -Y "%B%F{${c[24]}}app%f%b" -K _appscomplete dt
compctl -Y "%B%F{${c[24]}}app%f%b" -K _appscomplete mm

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

    # Ignore some of the largest tables
    IGNORE=(notifications_report laser_metatarget socialauth_linkedinconnection socialauth_linkedinuserprofile_connections mancx_reach)

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
        local workers=${GUNICORN_WORKERS:-9}
        local pid=${GUNICORN_PID:-/tmp/gunicorn.pid}
        local worker=${GUNICORN_WORKER}
        local timeout=${GUNICORN_REBOOT:-3}

        local cmd="gunicorn_django --workers=$workers --pid=$pid $worker $*"

        while true; do
            echo "$cmd\n"
            ${(z)cmd}

            echo -n "Sleeping $timeout: "
            for ((i = 0; i < timeout; i++)); do
                echo -n "."
                sleep 1
            done
            echo
        done
    else
        dm runserver 0.0.0.0:8000
    fi
}

function cdm {
    # Go to the code root, and follow the symlink if one.
    cd $(readlink -f $CODE_ROOT) # $CODE_ROOT:a isn't available in the older zsh's
}
