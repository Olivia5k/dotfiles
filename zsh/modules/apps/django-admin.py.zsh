export PYTHONSTARTUP=$XDG_DATA_HOME/python/djangoloader.py

alias dm='python2 manage.py'
alias ds='echo "no" | dm syncdb'
alias dz='bpython'
alias mm='django-admin.py makemessages -l en && django-admin.py compilemessages'

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

function dsr()##
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Remotely resync a Django database"
        elif [[ "$2" =~ "l(ong)?" ]] ; then
            local str="Same as dsh(), but uses and caches a remote mysqldump"
            echo $str
        fi
        return
    fi

    if [[ ! -f "$PWD/manage.py" ]]; then
        _zerror "No django manager found. Exiting"
        return 1
    fi

    DBTMP="tmp"
    DBHOST="dt"
    REMOTEDB="mancx_django"
    LOCALDB="dev_main"

    DATE=$(date +'%Y.%m.%d')
    F="$DBTMP/$DBHOST.$DATE.mysql"

    # Ignore laser tables and the large session table
    IGNORE=("django_admin_log" "django_session")
    if [[ "$1" != "--laser" ]]; then
        IGNORE+=("laser_metacategory" "laser_metacontroller" "laser_metaitem")
        IGNORE+=("laser_metalink" "laser_metamatch" "laser_metaqueue")
        IGNORE+=("laser_metatarget")
    fi

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

function dr()
{
    if [[ ! -f "manage.py" ]] ; then
        _zerror "No django manager found. Exiting"
        return 1
    fi

    rmext pyc &> /dev/null
    echo "Removed .pyc files"
    dm runserver 0.0.0.0:8000
}

function dcompile()
{
    setopt extendedglob
    if [[ ! -f "manage.py" ]] ; then
        _zerror "No django manager found. Exiting"
        return 1
    fi

    root=$PWD

    if [[ -n "$1" ]]; then
        hax="apps/{${*/ /,}}/locale"
    else
        hax="**/locale"
    fi

    echo "start"
    for d in "$(eval ${(z)hax})"; do
        echo $d
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
