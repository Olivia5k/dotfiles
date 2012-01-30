function chpwd() {
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

# Greh; this only works on very recent zshs :( Ugliest solution in the world,
# for now.
if [[ "${ZSH_VERSION[5,6]}" -ge 15 ]]; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs

    alias cdl="cdr -l"

    zstyle ':completion:*:*:cdr:*:*' menu selection
    zstyle ':chpwd:*' recent-dirs-file $XDG_DATA_HOME/zsh/cdr
    zstyle ':chpwd:*' recent-dirs-max 21
fi

zle -N _inline-updir
zle -N _inline-back

# ZLE hax0r navigation
function _inline-updir() {
    pushd -q ..
    zle .reset-prompt
}
function _inline-back() {
    if ! popd -q; then
        zle -M 'Directory stack empty'
        sleep 1
    fi
    zle .reset-prompt
}

bindkey "^[h" _inline-updir
bindkey "^[l" _inline-back
