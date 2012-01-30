# Give it to me
function grab() {
    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}:users "$@"
}

# Sudo give it to me.
function sgrab() {
    sudo chmod -R u=rwX,go=rX "$@"
    sudo chown -R ${USER}:users "$@"
}

# Give it to the warez
function wgrab() {
    sudo chmod -R 775 "$@"
    sudo chown -R root:warez "$@"
}

# Swap name of $1 and $2
function swap() {
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

# Spaces to dots.
function s2d() {
    zmv -v '(* *)' '${1// /.}'
}

# Backup a file
function bak() {
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
function hide() {
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
function mk() {
    mkdir $* && cd $1
}

# Remove all files of certain extension
function rmext() {
    rm -v **/*.$1
}

# Extract files from archives
function x() {
    if [[ -z "$1" ]] ; then
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
