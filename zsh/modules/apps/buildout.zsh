# Check if we actually are in a buildout environment
if [[ ! -f "bootstrap.py" ]] && [[ ! -f "bin/buildout" ]] ; then
    return
fi

function b()
{
    target=$1
    shift

    case $target in
        *)
            bin/buildout
        ;;

    esac
}

alias bdu="bin/develop update"
