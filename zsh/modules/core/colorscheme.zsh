function color()#
{
    if [[ -n "$1" ]]; then
        colorscheme=$1
    else
        colorscheme="default"
    fi

    if [[ ! -f "$ZSHCONFDIR/colorschemes/$colorscheme.zsh" ]]; then
        colorscheme="default"
    fi

    _zdebug "Setting colorscheme to $colorscheme"
    source $ZSHCONFDIR/colorschemes/$colorscheme.zsh
}
_colorcomplete()
{
    reply=()
    if (( CURRENT == 2 )) ; then
        for f in $ZSHCONFDIR/colorschemes/* ; do
            reply+=(${${f##*/}%\.*})
        done
    fi
}

# Completion \o/
compctl -Y "%B%F{${c[24]}}daemon%f%b" -K _colorcomplete color

if [[ -z "$ZCOLORSCHEME" ]]; then
    color $ZCOLOR
    export ZCOLORSCHEME=$ZCOLOR
fi
