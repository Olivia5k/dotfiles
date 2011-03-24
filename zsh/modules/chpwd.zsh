function chpwd()
{
    if [[ $UID = 0 ]] && [[ "^/srv" =~ $PWD ]] ; then
        echo
        print -P "   %B%F{red}:@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2%f%b"
        echo "   Don't be root in /srv. :@"
        echo
    fi

    if $HASTODO && [[ -f $TODOFILE ]] ; then
        todo && echo
    fi

    ls
}


function cd () {
    local opt=""
    if [[ ${+2} = 0 ]]; then
        if [[ -f $1 ]]; then
            builtin cd $opt $1:h
        else
            if ! builtin cd $opt $1 && [[ $#@ -eq 1 && ! -d $1 ]]; then
                echo -En "cd: $1 doesn't exist, do you want to create it? [y/N] "
                read -sq && mkcd $1
            fi
        fi
    else
        if [[ -z $3 ]]; then
            builtin cd $opt "$1" "$2"
        else
            echo cd: too many arguments
        fi
    fi
}

function mkcd () {
  test -z "$1" && echo >&2 "mkcd: no path given" && return
  test -d "$1" && echo >&2 "mkcd: Directory $1 already exists"
  mkdir -p -- "$1"
  cd -- "$1"
}

# cdr <3
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

alias cdl="cdr -l"

zstyle ':completion:*:*:cdr:*:*' menu selection
zstyle ':chpwd:*' recent-dirs-file $XDG_DATA_HOME/zsh/cdr
zstyle ':chpwd:*' recent-dirs-max 21
