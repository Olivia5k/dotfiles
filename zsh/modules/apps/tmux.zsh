alias et="cdc tmux && $EDITOR tmux.conf"
function attach() {
    if [[ -n "$TMUX" ]]; then
        _zerror "Already in tmux. Bailing."
        return 1
    fi

    if [[ -z "$1" ]] ; then
        _zerror "Argument required."
        return 1
    fi

    tmux attach -t $1
}
