zstyle :compinstall filename '~/.zshrc'

#autoload -Uz compinit
#compinit -d $COMPDUMP

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 2 numeric
zstyle ':completion:*:descriptions' format "%B${c[24]}%d%f%b:"
zstyle ':completion:*:directories' format "%B${c[25]}%d%f%b:"
zstyle ':completion:*:corrections' format "%B${c[26]}%d%b%f:"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path /tmp/.$USER.zshcompcache
zstyle ':completion:*' hosts off

# tab completion for PID :D
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

zstyle ':completion:*:*:psg:*' menu yes select
zstyle ':completion:*:psg:*' force-list always

# Make cd not select parent dir
zstyle ':completion:*:cd:*' ignore-parents parent pwd
