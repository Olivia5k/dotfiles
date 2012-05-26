bindkey '^[[OA' history-beginning-search-backward
bindkey '^[[OB' history-beginning-search-forward

# These three are mostly useful when on mobile connections and things are slow.
function cl() {
    cd $1 && ls
}

function cll() {
    cd $1 && ls -l
}

function clal() {
    cd $1 && ls -al
}

function tm() {
    if [[ -n "$1" ]] ; then
        if [[ "$1" =~ "[0-9]+" ]] ; then
            echo "TMOUT set to $1"
            export TMOUT=$1
        else
            zerror "TMOUT needs to be set to a numerical value."
        fi
    else
        if [[ -n "$TMOUT" ]] ; then
            echo "TMOUT unset"
            unset TMOUT
        else
            echo "TMOUT set to 90"
            export TMOUT=90 # TODO: Add variable in user.zsh
        fi
    fi
}

function rationalise-dot() {
    local reply REPLY REPLY2
    if [[ $_IS_PASTING = 1 ]]; then
        zle self-insert
        return
    fi
    #local MATCH
    if [[ $LBUFFER =~ '(^|/| |    |'$'\n''|\||;|&)\.\.$' ]]; then
        LBUFFER+=/
        zle self-insert
        zle self-insert

        split-shell-arguments
        (( REPLY -= 1 ))  # The actual $REPLY is whitespace. See doc for ssa.
        zle -M ${reply[$REPLY]:a}
    else
        zle self-insert
    fi
}

zle -N rationalise-dot
bindkey . rationalise-dot
# without this, typing a . aborts incremental history search
bindkey -M isearch . self-insert
