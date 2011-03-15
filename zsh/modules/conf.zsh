function cdc()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Change PWD relative to configuration files"
        fi
        return
    fi
    cd $CONFIG/$1
}

alias ez="cdc zsh && $EDITOR zshrc"
alias z=ez
