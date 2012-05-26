alias grep="grep $GREPOPTS"
alias :q="exit"
alias bell='echo -en "\007"'
alias pg="ping google.com -c 3"

# X11 specific aliases
if [[ -n "$DISPLAY" ]] ; then
    # Sets your keyboard to be snappier. Very recommended.
    alias xr='xset r rate 330 45 && echo 330/45'

    # Sets custom keymaps of both qwerty and dvorak
    alias xa='setxkbmap a6'
    alias xq='setxkbmap q6'
    alias aoeu="xq"
    alias asdf="xa"

    if has urxvt ; then
        alias u='urxvt &| exit'
        alias us='urxvt -name smallfont &| exit'
    fi
fi

# Count which commands you use the most. Give a numerical argument and it
# will print a list of that length.
# The aliases module is the best suited place for this one, really.
function lscmd()
{
    if [[ -n "$1" ]] ; then
        local c=$1
    else
        local c=25
    fi

    cat $LOGS/zsh.history.log |
    perl -pe 's/^: [0-9]+:[0-9]+;\s*//' |
    sort |
    uniq -c |
    sort -n -k1 |
    tail -$c |
    tac
}

insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[d" insert-sudo
