function my() {
    service=${1:-help}
    shift

    date=$(print -P '%D{%Y.%m.%d}')

    case $service in
        help)
            echo "helpful help is helpful"
        ;;

        dump)
            mysqldump ${1:--all-databases} > ${1:-full}.$date.mysql

            return $?
        ;;

        load)
            if [[ -z "$1" ]]; then
                zerror "mysqldump filename required"
                return 1
            fi

            print -P "Loading %B${c[23]}${1}%f%b"
            return
            if has pv; then
                pv -pte $1 | mysql
            else
                mysql < $1
            fi
            return $?
        ;;

        remote)
            if [[ -z "$1" ]]; then
                zerror "hostname required"
                return 1
            fi

            ssh $1 -t mysql $2
            return $?
        ;;

        remote_load)
            if [[ -z "$1" ]]; then
                zerror "hostname required"
                return 1
            fi

            host=$1
            db=${2:-mancx_django}

            f="$host.$db.$date.mysql"

            if [[ ! -f $f ]]; then
                print -P "Grabbing %B${c[19]}${db}%f%b from %B${c[27]}${host}%f%b"
                ssh $host mysqldump $db > $f || return 1
            fi

            # Use self!
            my load $f
            return $?
        ;;

        size)
            if [[ -z "$1" ]]; then
                zerror "hostname required"
                return 1
            fi

            q='SELECT table_name, ROUND(((data_length + index_length) / (1024*1024)),2) AS "MB" FROM information_schema.tables ORDER BY MB desc LIMIT 10'
            echo ssh $1 mysql -e \"${q}\"
        ;;
    esac
}
