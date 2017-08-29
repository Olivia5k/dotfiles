#!/usr/bin/zsh
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_DIRS=${XDG_CONFIG_DIRS:-/etc/xdg}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/share/:/usr/local/share/}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}

export LOGS="$HOME/.logs" ; mkdir -p $LOGS
export HISTFILE="$LOGS/.zsh_history"
export HISTSIZE=100000
export SAVEHIST=100000

export LD_LIBRARY_PATH="/usr/lib:/usr/local/lib"

export EDITOR="emacsclient"
export VISUAL="emacsclient"
export HOMEBIN="$HOME/.local/bin"
export GOPATH="$HOME"
local _PATH="$HOMEBIN:$GOPATH/bin"

# If $_PATH is not in $PATH, add it, but only once.
if ! [[ $PATH =~ "$_PATH" ]] ; then
  export PATH=$_PATH:$PATH
fi

zstyle ':completion:*' hosts off

source $HOME/.antigen.zsh
antigen use oh-my-zsh
antigen bundle zsh-users/zsh-syntax-highlighting
antigen theme gentoo
antigen bundle z; export _Z_CMD="j"

export ZSH_HIGHLIGHT_STYLES[alias]='fg=87,bold'
export ZSH_HIGHLIGHT_STYLES[command]='fg=103,bold'
export ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=103,bold'
export ZSH_HIGHLIGHT_STYLES[precommand]='fg=110,bold'
export ZSH_HIGHLIGHT_STYLES[builtin]='fg=65,bold'
export ZSH_HIGHLIGHT_STYLES[function]='fg=215,bold'
export ZSH_HIGHLIGHT_STYLES[path]='fg=68,bold'
export ZSH_HIGHLIGHT_STYLES[assign]='fg=43'

export ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=87'
export ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=132'
export ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=107,bold'
export ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=107,bold'

alias ls='command ls -bhv --color=yes --group-directories-first'
alias l='ls -l'
alias zz="source $HOME/.zshrc"

alias docker="sudo docker"
alias e=$EDITOR
alias z=keyboard-setup

alias -g G="| grep -i"
alias -g S="| sort"
alias -g SH="| sort -h"
alias -g SU='| sort -u'
alias -g SN='| sort -n'
alias -g W="| wc -l"
alias -g E="&| exit"
alias -g EE="&& exit"

# Disable Ctrl-q/s flow control.
stty -ixon

# Git setup
alias ga='git add'
alias gs='git status'
alias gc='git commit'
alias gp='git push'
alias gu="git pull"
alias gb='git branch -v'
alias gba='git branch -av'
alias gbv='git branch -vv'
alias gsb='git show-branch'
alias gf='git fetch'
function gfa {
    if ! git config -l | grep "remote.origin.fetch" | grep refs/pull &> /dev/null; then
        git config --add remote.origin.fetch "+refs/pull/*/head:refs/remotes/origin/pr/*"
        print -P "%F{71}%BAuto%b%f: Set up PR fetching"
    fi
    git fetch --all
}

alias gt="git stash"
alias gtl="git stash list"
alias gtp="git stash pop"
alias gd='git diff'
alias gdh='git diff HEAD'
alias gls="git ls-files"

# Logging
if (( $+commands[tig] )) ; then
  alias gl='tig'
else
  alias gl='git lg'
fi

# Relatively go up to a repository root
function gr() {
  cd $(git rev-parse --show-toplevel)
}

# Set up hub as an alias for git if it is installed
if (( $+commands[hub] )) ; then
  alias git=hub
  if type compdef >/dev/null; then
    compdef hub=git
  fi
fi
