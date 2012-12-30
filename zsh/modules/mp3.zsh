export STAGE_DIR="/warez/tmp/s2/Music"
export MOUNT_DIR="/warez/tmp/s2/mnt"
export TMP_DIR="/warez/tmp/s2/tmp"

function mp3() # Main interface
{
    arg=$1
    shift

    case $arg in
        sync)
            _mp3_sync ; ;;

        mount)
            _mp3_mount ; ;;

        umount)
            _mp3_umount ; ;;

        add)
            _mp3_add $* ; ;;

        delete)
            _mp3_delete $* ; ;;

        *)
            echo "Error: no command specified."
            echo "Use tab completion to find the commands."
            return 1
    esac
}

function _mp3() # Completion
{
    if (( CURRENT == 2 )); then
        _values 'core command' \
            'add[add releases to stage dir]' \
            'delete[remove releases from stage dir]' \
            'sync[rsync the stage dir]' \
            'mount[mount the device]' \
            'umount[umount the device]' \
            && return
    fi

    case $words[2] in
        add)
            _arguments "*:directories:_path_files -/" && return
        ;;
        delete)
            _arguments "*:directories:_path_files -/ -W $STAGE_DIR" && return
        ;;
    esac
}

function _mp3_sync()
{
    rsync -rvu --delete $STAGE_DIR $MOUNT_DIR --temp-dir=$TMP_DIR
}
function _mp3_mount()
{
    curlftpfs ftp://192.168.1.${1:-8}:2221 -o user=francis:francis $MOUNT_DIR
}
function _mp3_umount()
{
    fusermount -u -z $MOUNT_DIR
}
function _mp3_add()
{
    echo rsync -vru $* $STAGE_DIR
    rsync -vru $* $STAGE_DIR
}
function _mp3_delete()
{
    for d in $*; do
        rm -rv $STAGE_DIR/$d
    done
}

compdef _mp3 mp3
