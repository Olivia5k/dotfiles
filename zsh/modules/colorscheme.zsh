function color()#
{
    if [[ -n "$1" ]] ; then
        colorscheme=$1
    else
        colorscheme="default"
    fi

    if [[ ! -f $ZSHCONFDIR/colorschemes/$colorscheme.zsh ]]; then
        colorscheme="default"
    fi

    source $ZSHCONFDIR/colorschemes/$colorscheme.zsh
}

if [[ -z "$HASCOLOR" ]]; then
    color $ZCOLOR
    export HASCOLOR=$ZCOLOR
fi
