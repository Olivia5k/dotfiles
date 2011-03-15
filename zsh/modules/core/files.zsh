# Give it to me
function grab()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Set ownership of any given files to yourself."
        fi
        return
    fi

    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}:users "$@"
}

# Sudo give it to me.
function sgrab()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "sudo Set ownership of any given files to yourself."
        fi
        return
    fi

    sudo chmod -R u=rwX,go=rX "$@"
    sudo chown -R ${USER}:users "$@"
}

# Give it to the warez
function wgrab()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Set ownership of any given files to root:warez."
        fi
        return
    fi
    sudo chmod -R 775 "$@"
    sudo chown -R root:warez "$@"
}

# Swap name of $1 and $2
function swap()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo 'Swap name of first and second arguments'
        fi
        return
    fi

    if [[ -f $1 ]] ; then
        if [[ -f $2 ]] ; then
            mv $1 $1.swapcopy;
            mv $2 $1;
            mv $1.swapcopy $2;
        else
            echo "'$2' is not a valid file!"
        fi
    else
        echo "'$1' is not a valid file!"
    fi
}

function s2d()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo 'Substitute spaces with dots in filenames'
        fi
        return
    fi

    zmv -v '(* *)' '${1// /.}'
}

# Backup a file
function bak()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Backup a file or directory"
        fi
        return
    fi

    if [[ -z "$1" ]] ; then
        _zerror "No arguments given."
        return
    fi

    local ext=${$2:-".bak"}
    local dest="$1.$ext"
    if [[ -f $dest ]] || [[ -d $dest ]] ; then
        _zerror "Backup destination $dest already exists."
        return
    fi

    cp $1 $dest
}

# Hide or unhide files.
function hide()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Hide or unhide a file."
        fi
        return
    fi

    if [[ -z "$1" ]] ; then
        _zerror "No arguments given"
        return
    fi

    for f in $* ; do
        if [[ ! -f $f ]] ; then
            _zerror "$f: Not a file"
            continue
        elif [[ ! -w $f ]] ; then
            _zerror "$f: Not writable"
            continue
        fi

        local b=$(basename $f)
        local dir=$(dirname $f)
        if [[ $b =~ "^\." ]] ; then
            # File is hidden
            local dest="$dir/$(echo $b | sed -e 's/^\.//')"
        else
            # File is not hidden
            local dest="$dir/.$b"
        fi

        if [[ -f $dest ]] ; then
            _zerror "Destination file $dest exists"
            continue
        fi

        print -P "%B${f}%b -> %B${dest}%b"
        mv $f $dest
    done
}

# Make the directories and enter $1
function mk()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo 'Make a directory and enter it'
        fi
        return
    fi

    mkdir $* && cd $1
}

# Make the directories and enter $1
function cpmk()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo 'Make a directory and copy files to it'
        fi
        return
    fi

    echo mkdir ${*[-1]} && echo cp $*
}

# Remove all files of certain extension
function rmext()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Remove all files of certain extension"
        fi
        return
    fi

    rm -v **/*.$1
}

# Extract files from archives
function x()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Extract files from archives"
        fi
        return
    fi

    if [[ -n "$1" ]] ; then
        _zerror "No files given."
        return
    fi

    for f in $* ; do
        if [[ -f $f ]]; then
            case $f in
                *.tar.bz2) command tar xjf $f    ;;
                *.tar.gz)  command tar xzf $f    ;;
                *.bz2)     command bunzip2 $f    ;;
                *.rar)     command unrar x $f    ;;
                *.gz)      command gunzip $f     ;;
                *.tar)     command tar xf $f     ;;
                *.tbz2)    command tar xjf $f    ;;
                *.tgz)     command tar xzf $f    ;;
                *.zip)     command unzip $f      ;;
                *.Z)       command uncompress $f ;;
                *.7z)      command 7z x $f       ;;
                *.xz)      command unxz -vk $f   ;;
                *.lzma)    command unlzma -vk $f ;;
                *)         print "'$f' cannot be extracted via x()" ;;
            esac
        else
            print "'$f' is not a valid file"
        fi
    done
}

# Print a file without comments
alias cc='grep -Ev "^\s*(#|$)"'
