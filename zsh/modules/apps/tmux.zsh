function attach() {
    if [[ -n "$TMUX" ]]; then
        zerror "Already in tmux. Bailing."
        return 1
    fi

    if [[ -z "$1" ]] ; then
        zerror "Argument required."
        return 1
    fi

    tmux attach -t $1
}
