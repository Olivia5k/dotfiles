function my() {
    service=${1:-help}
    shift

    date=$(print -P '%D{%Y.%m.%d}')

    # TODO: Drop and recreate

    case $service in
        dump)
            # TODO: Query for db total size and use pv for visualization?
            mysqldump ${1:--all-databases} > ${1:-full}.$date.mysql

            return $?
        ;;

        load)
            if [[ -z "$1" ]]; then
                zerror "mysqldump filename required"
                return 1
            fi

            print -P "Loading %B${c[23]}${1}%f%b"

            if has pv; then
                pv -pte $1 | mysql
            else
                mysql < $1
            fi
            return $?
        ;;

        remote)
            ssh ${1:-dt} -t mysql $2
            return $?
        ;;

        remote_load)
            host=${1:-dt}
            db=${2:-mancx_django}

            f="$host.$db.$date.mysql"

            # TODO: See TODO on dump()
            if [[ ! -f $f ]]; then
                print -P "Grabbing %B${c[19]}${db}%f%b from %B${c[27]}${host}%f%b"
                ssh $host mysqldump $db > $f || return 1
            fi

            # Use self!
            my load $f
            return $?
        ;;

        size)
            mb="ROUND(((data_length + index_length) / (1024*1024)),2) AS MB"
            q="SELECT table_name, $mb FROM information_schema.tables "
            q+="ORDER BY MB desc LIMIT ${2:-10}"

            ssh ${1:-dt} mysql -e ${(qqqq)q} | column -t

            return $?
        ;;

        *)
            echo "helpful help is helpful"
            return 1
        ;;

    esac
}
