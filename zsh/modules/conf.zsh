function cdc() {
    cd $CONFIG/$1
}

alias ez="cdc zsh && $EDITOR zshrc"
alias z=ez
