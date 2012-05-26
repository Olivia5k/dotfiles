function f() {
    find 2>/dev/null | grep -is "$1"
}

function fd() {
    find 2>/dev/null -type d | grep -is "$1"
}

# Global grep. Searches files for content.
function gg() {
    grep "$*" * -RIins
}

# Global grep extended. Searches files for content with real regular expressions.
function gge() {
    grep "$*" * -ERIins
}

# Same as above, but does not print the matching line. Useful when searching
# through files with criminally long lines.
function ggl() {
    grep "$*" * -RlIis
}

# Same as above, but instead of printing the files, open them in your editor
function eggl() {
    if [[ $EDITOR = $(which vim) ]] ; then
        $EDITOR -p $(ggl $*)
    else
        $EDITOR $(ggl $*)
    fi
}

# List differential files between two directories
function dirdiff() {
    if [[ -d "$1" ]] && [[ -d "$1" ]] ; then
        _dirdiff $1 $2
        _dirdiff $2 $1
    else
        zerror "Both arguments must be valid directories"
    fi
}

function _dirdiff() {
    local listfile="/tmp/.zshlistfile"
    find $2 -type f > $listfile

    print -P "\n%BFiles in %F{${c[4]}}${1}%f not in %F{${c[4]}}${2}%f%b:"
    for i in $(find $1 -type f) ; do
        f=$(basename $i)
        if ! grep $f $listfile &> /dev/null; then
            echo $i
        fi
    done
    rm $listfile
}
