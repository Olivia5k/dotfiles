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

# cdr <3
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

alias cdl="cdr -l"

zstyle ':completion:*:*:cdr:*:*' menu selection
zstyle ':chpwd:*' recent-dirs-file $XDG_DATA_HOME/zsh/cdr
zstyle ':chpwd:*' recent-dirs-max 21
