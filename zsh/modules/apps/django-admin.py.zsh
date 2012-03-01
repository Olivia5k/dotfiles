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
    local cmd

    if [[ -n "$1" ]]; then
        cmd="apps/$1/tests.py"

        if [[ -n "$2" ]]; then
            cmd+=":$2"
            if [[ -n "$3" ]]; then
                cmd+=".$3"
            fi
        fi
    fi

    coverage run manage.py test $cmd
}

_testcomplete() {
    reply=()
    local module class in_class

    # Module
    if (( CURRENT == 2 )); then
        for f in ./apps/*(/); do
            # Only include those that actually have tests
            if [[ -f "$f/tests.py" ]]; then
                reply+=(${f##*/})
            fi
        done

    # Class
    elif (( CURRENT == 3 )); then
        split-shell-arguments
        module=$reply[4]  # Current module

        # Reset the reply array since ssa contaminates it
        reply=()

        # Loop all the lines in the tests file
        for line in ${(f)"$(<apps/$module/tests.py)"} ; do
            if [[ $line[1,5] = "class" ]]; then
                # Grab all lines that start with "class". Extract the name
                # only.
                reply+=(${${line#class }%%\(testtype\):})
            fi
        done

    # Function
    elif (( CURRENT == 4 )); then
        split-shell-arguments
        module=$reply[4]  # Current module
        class=$reply[6]  # Current class
        has_class=false

        # Reset the reply array since ssa contaminates it
        reply=()

        # Loop all the lines in the tests file
        for line in ${(f)"$(<apps/$module/tests.py)"} ; do
            # If we have found the class, examine the actual lines
            if $has_class; then
                if [[ $line[1,13] = '    def test_' ]]; then
                    # If the line begins as a test function defition, add it!
                    reply+=(${${line#    def }%%\(self\):})
                elif [[ $line[1,5] = "class" ]]; then
                    # If the line begins as a class, we have iterated over the
                    # whole class that we are completing for. Bail.
                    break
                fi

            # If we haven't found the class we are completing for yet, check if
            # we are about to!
            elif ! $has_class && [[ $line =~ "class $class\(" ]] ; then
                has_class=true
            fi
        done

    fi
}

_appscomplete() {
    reply=()

    for f in ./apps/*(/); do
        reply+=(${f##*/})
    done
}

# Completion \o/
compctl -Y "%B%F{${c[24]}}app%f%b" -K _testcomplete dt
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
