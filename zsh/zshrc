# zshrc by Lowe Thiderman (lowe.thiderman@gmail.com)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).
# https://github.com/daethorian/conf-zsh

# Variables and initialization {{{

# zsh configuration directory; dynamically found
export ZSHRC="$HOME/.zshrc"
export ZSHCONFDIR=$ZSHRC:A:h

# The XDG standard is indeed quite exquisite.
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_DIRS=${XDG_CONFIG_DIRS:-/etc/xdg}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/share/:/usr/local/share/}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}

# XDG misses a log specification :(
export LOGS="$HOME/.local/logs"
export HISTFILE="$LOGS/zsh.history.log"
export HISTSIZE=100000
export SAVEHIST=100000

export TCOLORS=$(echotc Co)

# Self compiled libraries
export LD_LIBRARY_PATH="/usr/lib:/usr/local/lib"

export PMODE=3
export EDITOR="vim"
export VISUAL="vim"
export MAIL="$HOME/var/mail"
export HOMEBIN="$HOME/.local/bin"
local _PATH="$HOME/bin:$HOMEBIN:$HOME/.gem/ruby/1.9.1/bin"

# If $_PATH is not in $PATH, add it, but only once.
if ! [[ $PATH =~ "$_PATH" ]] ; then
  export PATH=$_PATH:$PATH
fi

if [[ "$UID" != 0 ]]; then
  fpath=( $ZSHCONFDIR/completion "${fpath[@]}" )
fi

autoload -Uz compinit
compinit -d $XDG_DATA_HOME/zsh/compdump

autoload colors zsh/terminfo  # Colors
autoload -Uz vcs_info  # git integration
bindkey -e
setopt extendedglob
setopt sharehistory

# }}}
# Colors {{{
c=()
if [[ $TCOLORS = 256 ]]; then
  c+="%F{237}"     # 1.  Prompt decoration
  c+="%F{143}"     # 2.  Hostname
  c+="%F{160}"     # 3.  Hostname root
  c+="%F{067}"     # 4.  Directory
  c+="%F{096}"     # 5.  Directory non-multi
  c+="%F{196}"     # 6.  Error
  c+="%F{184}"     # 7.  Jobs
  c+="%F{067}"     # 8.  Clock
  c+="%F{195}"     # 9.  Mail
  c+="%F{062}"     # 10. Battery percent
  c+="%F{063}"     # 11. Battery timeleft
  c+="%F{196}"     # 12. TMOUT
  c+="%F{066}"     # 13. CVS System
  c+="%F{214}"     # 14. CVS Repo
  c+="%F{083}"     # 15. CVS Clean
  c+="%F{160}"     # 16. CVS Unstaged
  c+="%F{095}"     # 17. CVS Action
  c+="%F{202}"     # 18. CVS Staged / Debug
  c+="%F{120}"     # 19. Enabled / Success
  c+="%F{240}"     # 20. Disabled
  c+="%F{067}"     # 21. Doc: Function
  c+="%F{240}"     # 22. Doc: Undocumented
  c+="%F{148}"     # 23. Doc: File
  c+="%F{048}"     # 24. Comp: Descriptions
  c+="%F{067}"     # 25. Comp: Directories
  c+="%F{196}"     # 26. Comp: Corrections
  c+="%F{045}"     # 27. IP Adress
  c+="%F{123}"     # 28. CVS Super repo
else
  c+="%F{black}"   # 1.  Prompt decoration
  c+="%F{green}"   # 2.  Hostname
  c+="%F{red}"     # 3.  Hostname root
  c+="%F{blue}"    # 4.  Directory
  c+="%F{red}"     # 5.  Directory non-multi
  c+="%F{red}"     # 6.  Error
  c+="%F{yellow}"  # 7.  Jobs
  c+="%F{cyan}"    # 8.  Clock
  c+="%F{red}"     # 9.  Mail
  c+="%F{yellow}"  # 10. Battery percent
  c+="%F{magenta}"   # 11. Battery timeleft
  c+="%F{red}"     # 12. TMOUT
  c+="%F{yellow}"  # 13. CVS System
  c+="%F{yellow}"  # 14. CVS Repo
  c+="%F{green}"   # 15. CVS Clean
  c+="%F{red}"     # 16. CVS Unstaged
  c+="%F{cyan}"    # 17. CVS Action
  c+="%F{cyan}"    # 18. Debug
  c+="%F{green}"   # 19. Enabled / Success
  c+="%F{red}"     # 20. Disabled
  c+="%F{blue}"    # 21. Doc: Function
  c+="%F{white}"   # 22. Doc: Undocumented
  c+="%F{red}"     # 23. Doc: File
  c+="%F{green}"   # 24. Comp: Descriptions
  c+="%F{blue}"    # 25. Comp: Directories
  c+="%F{red}"     # 26. Comp: Corrections
  c+="%F{cyan}"    # 27. IP Adress
  c+="%F{cyan}"    # 28. CVS Super repo
fi

export c
# }}}
# Helper functions {{{

function zerror() {
  print -P "%B${c[6]}Error%f%b:" $*
}

function has() {
  [[ -x $commands[$1] ]] && return true # zsh style \o/
}

# }}}
# External modules {{{

# zsh-sysadmin. Quickfast!
source $ZSHCONFDIR/modules/filedb/zsys.zsh

if [[ "$TCOLORS" = 256 ]]; then
  # Shell syntax highlighting, in realtime.
  source $ZSHCONFDIR/modules/syntax/zsh-syntax-highlighting-filetypes.zsh

  # Really extensive LS_COLORS.
  eval $(dircolors -b $ZSHCONFDIR/modules/LS_COLORS/LS_COLORS)
fi

# }}}
# Basic configuration {{{

# aliases {{{

# The most useful alias there ever was
alias zz="source $ZSHRC"

export GREPOPTS='--color=auto'
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
fi

# Count which commands you use the most. Give a numerical argument and it
# will print a list of that length.
# The aliases module is the best suited place for this one, really.
function lscmd()
{
  if [[ -n "$1" ]] ; then
    local count=$1
  else
    local count=25
  fi

  cat $LOGS/zsh.history.log |
  perl -pe 's/^: [0-9]+:[0-9]+;\s*//' |
  sort |
  uniq -c |
  sort -n -k1 |
  tail -$count |
  tac
}

insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[d" insert-sudo

# }}}
# colors {{{

# Prints a table of the 16 default colors. Usually comes as a standalone
# script, but hey, why not integrate it? :>
function cs() {
  T='HaX' # The test text
  echo -e "         40m   41m   42m   43m   44m   45m   46m   47m";

  for FGs in '  m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
    '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
    '  36m' '1;36m' '  37m' '1;37m';
  do
    FG=${FGs// /}
    echo -en " $FGs \033[$FG  $T  "
    for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
      # srsly, where does EINS come from? :(
      do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
    done
    echo;
  done
}

# Prints a table of the 256 extended colors. Incredibly useful when making
# terminal editor colorschemes. The default prints quite wide, so when in a
# small terminal, it prints a simplified version. Given any arguments, the large
# one is printed regardless.
function CS() {
  for line in {0..15}; do
    for col in {0..15}; do
      local code=$(( $col * 16 + ${line} ))
      printf $'\e[38;05;%dm %03d' ${code} ${code}
    done
  print ; done
}

function CC() {
  for i in {16..255}; do
    echo -en "\e[38;5;${i}mColor $i\t"
    [ $((++j%6)) -eq 0 ] && echo
  done
}

# }}}
# completion {{{

zstyle :compinstall filename '~/.zshrc'

#autoload -Uz compinit
#compinit -d $COMPDUMP

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 2 numeric
zstyle ':completion:*:descriptions' format "%B${c[24]}%d%f%b:"
zstyle ':completion:*:directories' format "%B${c[25]}%d%f%b:"
zstyle ':completion:*:corrections' format "%B${c[26]}%d%b%f:"
zstyle ':completion:*:warnings' format "%B${c[26]}No matches for%b%f: %d"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path /tmp/.$USER.zshcompcache
zstyle ':completion:*' hosts off
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# tab completion for PID :D
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

zstyle ':completion:*:*:psg:*' menu yes select
zstyle ':completion:*:psg:*' force-list always

# Make cd not select parent dir
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# }}}
# dump {{{

function dump() {
  if [[ -z "$1"  ]]; then
    zerror "fielnaem pls"
    return 1
  fi

  if [[ ! -f "$1" ]]; then
    zerror "$1: no such file"
    return 127
  fi

  d="/srv/dump"
  url="http://dump.ninjaloot.se"
  if [[ -f "$d/$1" ]]; then
    zerror "$1 already exists at $url/$1."
    return 2
  fi

  cp $1 $d
  echo $url/$1
  if [[ -n "$DISPLAY" ]]; then
    echo $url/$1 | xclip
  fi
}

# }}}
# failsafe {{{

function fail() {
  if has figlet; then
    figlet :@ && echo
  else
    # Print red angry text
    echo -e "\n\033[1;31m   YOU'RE DOING IT WRONG! :@\033[0m\n"
  fi
}

alias sl='fail && ls'
alias ös='fail && ls'
alias lös='fail && ls'
alias xs='fail && cd'
alias vf='fail && cd'
alias cd..='fail && cd ..'
alias vom='fail && vim'
alias bim='fail && vim'
alias cim='fail && vim'
alias vum='fail && vim'
alias shitdown='fail && shutdown' # This has actually happened. Hilarity ensued.

# }}}
# files {{{

# Give it to me
function grab() {
  chmod -R u=rwX,go=rX "$@"
  chown -R ${USER}:users "$@"
}

# Sudo give it to me.
function sgrab() {
  sudo chmod -R u=rwX,go=rX "$@"
  sudo chown -R ${USER}:users "$@"
}

# Swap name of $1 and $2
function swap() {
  if [[ -f $1 ]] ; then
    if [[ -f $2 ]] ; then
      mv $1 $1.swapcopy;
      mv $2 $1;
      mv $1.swapcopy $2;
    else
      echo "'$2' is not a valid file!"
    fi
  else
    echo "'$1' is not a valid file!"
  fi
}

function spaces_to_dots() {
  zmv -v '(* *)' '${1// /.}'
}
alias s2d="spaces_to_dots"

# Backup a file
function bak() {
  if [[ -z "$1" ]] ; then
    zerror "No arguments given."
    return
  fi

  local ext=${$2:-.bak}
  local dest="$1.$ext"
  if [[ -f $dest ]] || [[ -d $dest ]] ; then
    zerror "Backup destination $dest already exists."
    return
  fi

  cp $1 $dest
}

# Hide or unhide files.
function hide() {
  if [[ -z "$1" ]] ; then
    zerror "No arguments given"
    return
  fi

  for f in $* ; do
    if [[ ! -f $f ]] ; then
      zerror "$f: Not a file"
      continue
    elif [[ ! -w $f ]] ; then
      zerror "$f: Not writable"
      continue
    fi

    local b=$(basename $f)
    local dir=$(dirname $f)
    if [[ $b =~ "^\." ]] ; then
      # File is hidden
      local dest="$dir/$(echo $b | sed -e 's/^\.//')"
    else
      # File is not hidden
      local dest="$dir/.$b"
    fi

    if [[ -f $dest ]] ; then
      zerror "Destination file $dest exists"
      continue
    fi

    print -P "%B${f}%b -> %B${dest}%b"
    mv $f $dest
  done
}

# Remove all files of certain extension
function rmext() {
  rm -v **/*.$1
}

# Extract files from archives
function x() {
  if [[ -z "$1" ]] ; then
    zerror "No files given."
    return
  fi

  for f in $* ; do
    if [[ -f $f ]]; then
      case $f in
        *.tar.bz2) command tar xjf $f  ;;
        *.tar.gz)  command tar xzf $f  ;;
        *.bz2)   command bunzip2 $f  ;;
        *.rar)   command unrar x $f  ;;
        *.gz)    command gunzip $f   ;;
        *.tar)   command tar xf $f   ;;
        *.tbz2)  command tar xjf $f  ;;
        *.tgz)   command tar xzf $f  ;;
        *.zip)   command unzip $f    ;;
        *.Z)     command uncompress $f ;;
        *.7z)    command 7z x $f     ;;
        *.xz)    command unxz -vk $f   ;;
        *.lzma)  command unlzma -vk $f ;;
        *)     print "'$f' cannot be extracted via x()" ;;
      esac
    else
      print "'$f' is not a valid file"
    fi
  done
}

# Print a file without comments
alias cc='grep -Ev "^\s*(#|$)"'

# }}}
# global aliases {{{

alias -g G="| grep -i"
alias -g M="| most"
alias -g L="| less"
alias -g X="| xargs"
alias -g H="| head"
alias -g T="| tail"
alias -g S="| sort"
alias -g SU='| sort -u'
alias -g SN='| sort -n'
alias -g SNR='| sort -nr'
alias -g W="| wc -l"
alias -g E="&| exit"
alias -g EE="&& exit"
alias -g N="&> /dev/null"
alias -g 1N="1> /dev/null"
alias -g 2N="2> /dev/null"

# }}}
# infect {{{

function infect() {
  if [[ -f $HOME/.infect ]]; then
    cd -q $(<$HOME/.infect)
    ./infect $*
    cd -q -
  else
    zerror "Infect file missing."
  fi
}

# }}}
# install {{{

function ins() {
  f=$(readlink -f $1)
  if [[ -f $f ]] && [[ -x $f ]] ; then
    if [[ -n "$2" ]] ; then
      local target=$2
    else
      local target=$(basename $1)
    fi

    if ! has $target ; then
      print -P "Installing %B${c[19]}$(basename $1)%f%b as %B${c[19]}${target}%f%b"
      ln -s $f $HOMEBIN/$target
    else
      zerror "$target is already installed"
    fi
  else
    zerror "$(basename $f) is not an executable file"
  fi
}

function unins() {
  f=$HOMEBIN/$(basename $1)
  if [[ -L $f ]] ; then
    print -P "Uninstalling %B${c[19]}$(basename $1)%f%b."
    unlink $f
  else
    zerror "$(basename $f) is not installed"
  fi
}

# }}}
# mp3 {{{

export STAGE_DIR="/warez/tmp/s2/Music"
export MOUNT_DIR="/warez/tmp/s2/mnt"
export TMP_DIR="/warez/tmp/s2/tmp"

function mp3() # Main interface
{
  arg=$1
  shift

  case $arg in
    sync)
      _mp3_sync ; ;;

    mount)
      _mp3_mount ; ;;

    umount)
      _mp3_umount ; ;;

    add)
      _mp3_add $* ; ;;

    delete)
      _mp3_delete $* ; ;;

    *)
      echo "Error: no command specified."
      echo "Use tab completion to find the commands."
      return 1
  esac
}

function _mp3() # Completion
{
  if (( CURRENT == 2 )); then
    _values 'core command' \
      'add[add releases to stage dir]' \
      'delete[remove releases from stage dir]' \
      'sync[rsync the stage dir]' \
      'mount[mount the device]' \
      'umount[umount the device]' \
      && return
  fi

  case $words[2] in
    add)
      _arguments "*:directories:_path_files -/" && return
    ;;
    delete)
      _arguments "*:directories:_path_files -/ -W $STAGE_DIR" && return
    ;;
  esac
}

function _mp3_sync()
{
  rsync -rvu --delete $STAGE_DIR $MOUNT_DIR --temp-dir=$TMP_DIR
}
function _mp3_mount()
{
  curlftpfs ftp://192.168.1.${1:-8}:2221 -o user=francis:francis $MOUNT_DIR
}
function _mp3_umount()
{
  fusermount -u -z $MOUNT_DIR
}
function _mp3_add()
{
  echo rsync -vru $* $STAGE_DIR
  rsync -vru $* $STAGE_DIR
}
function _mp3_delete()
{
  for d in $*; do
    rm -rv $STAGE_DIR/$d
  done
}

compdef _mp3 mp3

# }}}
# navigation {{{

bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward

function rationalise-dot() {
  local reply REPLY REPLY2
  if [[ $_IS_PASTING = 1 ]]; then
    zle self-insert
    return
  fi
  #local MATCH
  if [[ $LBUFFER =~ '(^|/| |  |'$'\n''|\||;|&)\.\.$' ]]; then
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

function mkcd() {
  mkdir -p $1 &> /dev/null
  cd $1
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

# Greh; this only works on very recent zshs! Check if chpwd_recent_dirs
# actually is loadable!
a=( ${^fpath}/chpwd_recent_dirs(N) )
if (( $#a > 0 )); then
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
    sleep 0.5
  fi
  zle .reset-prompt
}

bindkey "^[h" _inline-updir
bindkey "^[l" _inline-back

# }}}
# root {{{

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

# Kill root after $ROOT_TIMEOUT seconds
if [[ "$UID" = 0 ]] && [[ -n "$ROOT_TIMEOUT" ]] ; then
  print -P "Warning: Root shell will timeout after %B${c[12]}$ROOT_TIMEOUT seconds%f%b."
  TMOUT=$ROOT_TIMEOUT
fi

# }}}
# search {{{

function _clearbelowprompt() {
  zle -M ""
}

function _show_surroundings() {
  #zle -M ${"$(eval 'for a in $history[(I)<'$((HISTNO-10))-$((HISTNO+10))'>]; do if [[ $a -eq $HISTNO ]]; then printf '\''\n%s: %s\n\n'\'' $a $history[$a]; else; printf '\''%s: %s\n'\'' $a $history[$a]; fi; done')"}
  #zle -M ${"$(eval 'for a in $history[(I)<'$((HISTNO-10))-$((HISTNO+10))'>]; do printf '\''%s: %s\n'\'' $a $history[$a]; done')"}
  (( $LINES < 5 )) && { zle -M ""; return }
  local bound line
  typeset -a output
  typeset -A star
  bound=${NUMERIC:-$(( LINES < 10 ? 1 : LINES / 3 ))}
  star[$HISTNO]="*"
  for ((i = HISTNO - $bound; i < HISTNO + $bound && i < HISTCMD; i++)); do
    line="${${:-$star[$i]$i: $history[$i]}[1,COLUMNS-1]}"
    while (( ${(m)#line} > COLUMNS-1 )); do
    line[-1]=
    #for broken zsh#line=$line[1,-2]
    done
    output=( $line $output )
  done
  zle -M ${(pj:\n:)output}
}
zle -N zle-isearch-update _show_surroundings
zle -N zle-isearch-exit _clearbelowprompt

# }}}
# system {{{

alias shutdown='sudo shutdown -h now'
alias reboot='sudo reboot'

# grep processes and retain grep color
function psg() {
  if [[ -z "$1" ]] ; then
    zerror "Arguments plx"
    return
  fi

  ps haux | grep -i $1 | grep -Ev '(grep)' | grep -i $1
}

# control daemons: d <daemon> <action>
function d() {
  if [[ -z "$1" ]] ; then
    p="/var/run/daemons"
    for f in $(ls /etc/rc.d/*(.x)) ; do
      d=${f##*/}
      if [[ -f "$p/$d" ]] ; then
        co=${c[19]}
      else
        co=${c[20]}
      fi
      print -P "%B${co}${d}%f%b"
    done
    return
  fi

  cmd=${2:-restart}

  if [[ "$UID" = 0 ]] ; then
    /etc/rc.d/$1 $cmd
  else
    sudo /etc/rc.d/$1 $cmd
  fi
}

_daemoncomplete() {
  reply=()
  if (( CURRENT == 2 )) ; then
    for f in $(ls /etc/rc.d/*(.x)) ; do
      reply+=(${f##*/})
    done
  else
    reply=(start stop reload restart)
  fi
}

# Completion \o/
compctl -Y "%B${c[24]}daemon%f%b" -K _daemoncomplete d

# Control wireless network
function nr() {
  wlan=${1:-ninjanet}
  sudo netcfg -d $wlan &> /dev/null
  sudo netcfg $wlan
}

# Print IP address info.
function ips() {
  for i in $(ifconfig -a | grep -Eo "^[a-z0-9]+"); do
    ifdata=$(ifconfig $i)

    ip=$(echo $ifdata | grep -Eo "inet (addr:)?[0-9.]* " | grep -Eo "([0-9]+\.?){4}")
    ip6=$(echo $ifdata | grep -Eo "inet6 (addr:)? [a-f0-9:/]* " | grep -Eo "[a-f0-9:/]+ $")

    if [[ -n "$ip" ]] ; then
      s="${c[19]}%B${i}%b%f: ${c[27]}%B${ip}%b%f"
      if [[ -n "$ip6" ]] ; then
        s+=" and ${c[27]}%B${ip6}%b%f"
      fi
      print -P $s
    else
      print -P "${c[20]}%B${i}%b%f"
    fi
  done

  # Thank you Loopia! \o/
  pub=$(curl http://dns.loopia.se/checkip/checkip.php 2> /dev/null | grep -Eo "([0-9]+\.?){4}")

  if [[ -n "$pub" ]] ; then
    print -P "${c[19]}%BPublic%b%f: ${c[27]}%B${pub}%b%f"
  else
    print -P "${c[20]}%BPublic%b%f"
  fi

  avahi=$(ps aux | grep "avahi-daemon: running" | grep -Eo "\[.*\]" | tr -d "[]")
  if [[ -n "$avahi" ]] ; then
    print -P "${c[19]}%BAvahi%b%f: ${c[27]}%B${avahi}%b%f"
  fi
}

# Kill based on port!
function kill_port() {
  port=${1:-8000}
  sig=${2:-9}

  d="[[:digit:]]"
  b="[[:blank:]]"

  pid=$(netstat -apn 2> /dev/null | grep ":${port}${b}" | grep -Eo "$d+/")
  pid=${pid%%/}

  if [[ -z "$pid" ]] ; then
    echo "No process found at $port"
    return
  fi

  kill -$sig $pid
}
alias kp="kill_port"

# }}}

# }}}
# Application specific {{{

# django {{{

autoload -U regexp-replace
setopt re_match_pcre

# Needed for uno because uno is sämst :(
export URE_BOOTSTRAP="file:///usr/lib/libreoffice/program/fundamentalrc"
export PYTHONPATH="$PYTHONPATH:/usr/lib/libreoffice/basis-link/program/"

export DBTMP="tmp"
export DBHOST="dt"
export REMOTEDB="mancx_django"
export LOCALDB="mancx_django"
export CODE_ROOT="/srv/live"

alias dm='python2 manage.py'
alias dz='ipython'

function mm() {
  if [[ -n "$1" ]]; then
    while [[ -n "$1" ]]; do
      _mm apps/$1/
      shift
    done
  else
    for dir in apps/*(/) ; do
      if [[ -d $dir/locale  ]]; then
        _mm $dir
      fi
    done
  fi
}

function _mm() {
  cd -q $1

  django-admin.py makemessages -l en
  django-admin.py compilemessages

  cd -q ../..
}

function dt() {
  local cmd

  if [[ -n "$1" ]] && ! [[ $1 =~ '^--' ]]; then
    cmd="market/apps/$1/tests"
    shift

    if [[ -n "$1" ]] && ! [[ $1 =~ '^--' ]]; then
      cmd+="/$1.py"
      shift
      if [[ -n "$1" ]] && ! [[ $1 =~ '^--' ]]; then
        cmd+=":$1"
        shift
        if [[ -n "$1" ]] && ! [[ $1 =~ '^--' ]]; then
          cmd+=".$1"
          shift
        fi
      fi
    fi
  fi

  bin/test $cmd $*
}

function dtc() {
  local cmd

  if [[ -z "$1" ]]; then
    echo "Module plz"
  fi

  files=(apps/$1/**/*.py)
  coverage report $files
}

_testcomplete() {
  reply=()
  local module class in_class

  # Module
  if (( CURRENT == 2 )); then
    for f in ./market/apps/*(/); do
      # Only include those that actually have tests
      if [[ -d "$f/tests" ]]; then
        reply+=(${f##*/})
      fi
    done

  # Kind
  elif (( CURRENT == 3 )); then
    reply=(integration selenium unit)

  # Class
  elif (( CURRENT == 4 )); then
    split-shell-arguments
    module=$reply[4]  # Current module
    file=$reply[6]

    # Reset the reply array since ssa contaminates it
    reply=()

    # Loop all the lines in the tests file
    for line in ${(f)"$(<market/apps/$module/tests/$file.py)"} ; do
      if [[ $line[1,5] = "class" ]]; then
        # Grab all lines that start with "class". Extract the name
        # only.
        line=${line#class }
        regexp-replace line '\(.*' ""
        reply+=($line)
      fi
    done

  # Function
  elif (( CURRENT == 5 )); then
    split-shell-arguments
    module=$reply[4]  # Current module
    file=$reply[6]
    class=$reply[8]  # Current class
    has_class=false

    # Reset the reply array since ssa contaminates it
    reply=()

    # Loop all the lines in the tests file
    for line in ${(f)"$(<market/apps/$module/tests/$file.py)"} ; do
      # If we have found the class, examine the actual lines
      if $has_class; then
        if [[ $line[1,13] = '  def test_' ]]; then
          # If the line begins as a test function defition, add it!
          reply+=(${${line#  def }%%\(self\):})
        elif [[ $line[1,5] = "class" ]]; then
          # If the line begins as a class, we have iterated over the
          # whole class that we are completing for. Bail.
          break
        fi

      # If we haven't found the class we are completing for yet, check if
      # we are about to!
      elif ! $has_class && [[ $line =~ "class $class\(" ]] ; then
        has_class=true
      fi
    done

  fi
}

_appscomplete() {
  reply=()

  for f in ./apps/*(/); do
    reply+=(${f##*/})
  done
}

# Completion \o/
compctl -Y "%B${c[24]}app%f%b" -K _testcomplete dt
compctl -Y "%B${c[24]}app%f%b" -K _testcomplete dtc
compctl -Y "%B${c[24]}app%f%b" -K _appscomplete mm

function dsh() {
  dm=${1:-python2 bin/market}
  mysql -e "drop database $LOCALDB ; create database $LOCALDB character set utf8 collate utf8_general_ci;"
  echo "no" | ${(z)dm} syncdb
}

function dsr() {
  if [[ ! -f "$PWD/manage.py" ]]; then
    zerror "No django manager found. Exiting"
    return 1
  fi

  DATE=$(date +'%Y.%m.%d')
  F="$DBTMP/$DBHOST.$DATE.mysql"

  # Ignore some of the largest tables
  IGNORE=(notifications_report laser_metatarget socialauth_linkedinconnection socialauth_linkedinuserprofile_connections mancx_reach)

  i=""
  for x in $IGNORE ; do
    i+="--ignore-table=$REMOTEDB.$x "
  done

  if [[ ! -f $F ]] || [[ -n "$1" ]]; then
    echo "Grabbing $REMOTEDB from $DBHOST"
    ssh $DBHOST mysqldump $REMOTEDB $i > $F
  fi

  echo "Loading $F into $LOCALDB"
  mysql $LOCALDB < $F
}

function dr() {
  if [[ ! -f "manage.py" ]] ; then
    zerror "No django manager found. Exiting"
    return 1
  fi


  if has gunicorn_django; then
    local workers=${GUNICORN_WORKERS:-9}
    local pid=${GUNICORN_PID:-/tmp/gunicorn.pid}
    local worker=${GUNICORN_WORKER}
    local timeout=${GUNICORN_REBOOT:-3}

    local cmd="gunicorn_django --workers=$workers --pid=$pid $worker $*"

    while true; do
      echo -e "\nRemoving .pyc files"
      rmext pyc &> /dev/null
      echo -e "Removed .pyc files"

      echo "$cmd\n"
      ${(z)cmd}

      echo -n "Sleeping $timeout: "
      for ((i = 0; i < timeout; i++)); do
        echo -n "."
        sleep 1
      done
      echo
    done
  else
    while true ; do
      local timeout=3
      echo -e "\nRemoving .pyc files"
      rmext pyc &> /dev/null
      echo -e "Removed .pyc files"

      dm runserver 0.0.0.0:8000 --traceback
      echo -n "Sleeping $timeout: "
      for ((i = 0; i < timeout; i++)); do
        echo -n "."
        sleep 1
      done
      echo
    done
  fi
}

# Selenium Xvfb setup
alias selenium_xvfb="Xvfb :99 -ac -screen 0 1024x768x8"
alias sx="selenium_xvfb"

# }}}
# find {{{

function f() {
  find 2>/dev/null | grep -is "$1"
}

function fd() {
  find 2>/dev/null -type d | grep -is "$1"
}

# Global grep. Searches files for content.
function gg() {
  grep "$*" * -RIins
}

# Global grep extended. Searches files for content with real regular expressions.
function gge() {
  grep "$*" * -ERIins
}

# Same as above, but does not print the matching line. Useful when searching
# through files with criminally long lines.
function ggl() {
  grep "$*" * -RlIis
}

# Same as above, but instead of printing the files, open them in your editor
function eggl() {
  if [[ $EDITOR = $(which vim) ]] ; then
    $EDITOR -p $(ggl $*)
  else
    $EDITOR $(ggl $*)
  fi
}

# }}}
# git {{{

# Committing / General
alias ga='git add'
alias gs='git status'
alias gss='git status --short'
alias gc='git commit'
alias gca='gc --all'
alias gp='git push'

alias gu="git pull"

# No pull on production.
if [[ -f "$HOME/.local/production" ]]; then
  alias gu="zerror 'This is a production server. You cannot do that.' ;  return 1"
fi

alias gpp='git push origin'
alias guu='git pull origin'

# Branching (only really useful with -v, really)
alias gb='git branch -v'
alias gba='git branch -av'
alias gbv='git branch -vv'
alias gbav='git branch -avv'

# Remotes
alias gre='git remote -v'
alias gra='git remote add'

# Checkouting
alias go='git checkout'
alias goo='git checkout --ours'
alias got='git checkout --theirs'

# Stashing
alias gt="git stash"
alias gtl="git stash list"
alias gtp="git stash pop"
alias gts="git stash show"
# Since stashes are pretty irrevokably lost if dropped, aliases for dropping
# was skipped

# Submodules
alias gsa='git submodule add'
alias gsi='git submodule init'
alias gsu='git submodule update'
alias gus='git pull && gsu'

# Diffing
alias gd='git diff'
alias gdh='git diff HEAD'
alias gdt='git difftool'

# Merging
alias gm="git merge"
alias gmt="git mergetool"
alias gls="git ls-files"
alias glsu="git ls-files --unmerged"
alias glsm="git ls-files --modified"
alias glss="git ls-files --stage"
alias glsd="git ls-files --deleted"

# Logging
alias gl='git log --abbrev-commit --pretty=oneline --decorate'
alias gll='git log --abbrev-commit --decorate --stat'
alias glg="gl --graph"
alias gllg="gll --graph"
alias glc="git shortlog --summary --numbered"
alias glr="git reflog"

alias gau='git update-index --assume-unchanged'


# Setup remote for a branch (mnemonic: git branch remote)
function gbr() {
  if [[ -n "$1" ]]; then
    branch=$1
    remote=${2:=origin}  # Optionally; what remote?

    git config branch.$branch.remote $remote
    git config branch.$branch.merge refs/heads/$1

    echo "Branch $branch now tracking $remote"
  else
    zerror "Tell me a branch, fool."
  fi
}

# Setup upstream for a branch (mnemonic: git branch upstream)
function gbu() {
  if [[ -n "$1" ]] && [[ -n "$2" ]]; then
    branch=$1
    remote=$2
    remote_branch=${3:=${1}}  # Optionally; which remote branch?

    git branch $branch --set-upstream $remote/$remote_branch
  else
    zerror "Tell me a branch and a remote, fool."
  fi
}

# Relatively go up to a repository root
function gr() {
  cur=$PWD
  found=false
  is_in=false

  if [[ -r "$cur/.git" ]]; then
    is_in=true
    cur=${cur%/*}
  fi

  until [[ -z "$cur" ]]; do
    if [[ -r "$cur/.git" ]]; then
      found=true
      break
    fi
    cur=${cur%/*}
  done

  if $found; then
    if $is_in; then
      echo "In submodule: going to superproject"
    fi

    echo $cur
    cd $cur
  elif [[ -d "$PWD/.git" ]]; then
    echo "Already at project root"
  else
    zerror "Currently not in a git repository"
  fi
}

# Love borrowed from mikachu!
zle -N _gitref
zle -N _quote_word
zle -N _unquote_word
bindkey "^[g"  _gitref

autoload -U modify-current-argument
autoload -U split-shell-arguments

function _quote_word()
{
  local q=qqqq
  modify-current-argument '${('$q[1,${NUMERIC:-1}]')ARG}'
}

function _unquote_word()
{
  modify-current-argument '${(Q)ARG}'
}

function _quote_unquote_word()
{
  local q=qqqq
  modify-current-argument '${('$q[1,${NUMERIC:-1}]')${(Q)ARG}}'
}
function _split_shell_arguments_under() {
  local -a reply
  split-shell-arguments
  #have to duplicate some of modify-current-argument to get the word
  #_under_ the cursor, not after.
  setopt localoptions noksharrays multibyte
  if (( REPLY > 1 )); then
    if (( REPLY & 1 )); then
      (( REPLY-- ))
    fi
  fi
  REPLY=${reply[$REPLY]}
}

function _gitref() {
  local REPLY
  local msg="Select one of s, S, t, a, c, v, q, Q, r${${1+.}:-, h (help).}"
  zle -R $msg
  read -k
  case $REPLY in
    (s)
      modify-current-argument '$(git rev-parse --short='${NUMERIC:-4}' ${(Q)ARG} 2> /dev/null)'
      ;;
    (S)
      modify-current-argument '$(git rev-parse ${(Q)ARG} 2> /dev/null)'
      ;;
    (t)
      modify-current-argument '$(git describe --tags ${(Q)ARG} 2> /dev/null)'
      ;;
    (a)
      modify-current-argument '$(git describe --all ${(Q)ARG} 2> /dev/null)'
      ;;
    (c)
      modify-current-argument '${(q)$(git describe --contains ${(Q)ARG} 2> /dev/null)}'
      ;;
    (v)
      if [[ -d $(git rev-parse --show-cdup).git/svn ]]; then
        modify-current-argument '$(git rev-parse --short='${NUMERIC:-4}' $(git svn find-rev r$ARG 2> /dev/null) 2> /dev/null || git svn find-rev $ARG 2> /dev/null)'
      else
        zle -R "Not a git-svn repo."
      fi
      ;;
    (q)
      _quote_word
      ;;
    (Q)
      _unquote_word
      ;;
    (w)
      modify-current-argument '$(git rev-list $ARG 2> /dev/null | wc -l)'
      ;;
    (r)
      local -a match mbegin mend
      modify-current-argument '${ARG//(#b)((#B)(*[^.])#)(#b)(.(#c2,3))((#B)([^.]*)#)/$match[3]$match[2]$match[1]}'
      ;;
    (h)
      [[ $1 = help ]] && return
      _split_shell_arguments_under
      word=$REPLY
      zle -M \
"
s: convert word to sha1, takes numeric argument (default: 4) ($(git rev-parse --short=${NUMERIC:-4} $word 2> /dev/null))
S: convert word to full length sha1 ($(git rev-parse $word 2> /dev/null))
t: convert word to described tag ($(git describe --tags $word 2> /dev/null))
a: convert word to described ref ($(git describe --all $word 2> /dev/null))
c: convert word to contained tag (${(q)$(git describe --contains $word 2> /dev/null)})
v: convert word with git svn find-rev
q: quote word
Q: unquote word
r: change order of range from a..b or a...b to b..a or b...a respectively
h: this help message"
      _gitref help
      ;;
    (*)
      [[ -n $REPLY ]] && repeat ${NUMERIC:-1}; do zle -U - $REPLY; done
      ;;
  esac
  zle -R -c
}

function _find_git_root() {
  # Helper that finds the real git root.
  # Useful when relatively needing data from a new-style submodule.
  cur=${1:-$PWD}
  until [[ -z "$cur" ]]; do
    if [[ -f "$cur/.git" ]]; then
      # New-style submodules are files
      rel=${${(s: :)"$(<$cur/.git)"}[2]}
      if [[ "$rel" =~ "^../" ]]; then
        # Relative relative! D:
        combined="$cur/$rel"
        git_root=$combined:A
      else
        # Absolute relative.
        git_root=$rel
      fi

      break
    elif [[ -d "$cur/.git" ]]; then
      git_root="$cur/.git"
      break
    fi
    cur=${cur%/*}
  done

  if [[ "$git_root" = "/" ]]; then
    git_root=""
  fi
}

if has hub; then
  alias git=hub
  if type compdef >/dev/null; then
    compdef hub=git
  fi
fi

# }}}
# ls {{{

export LSOPTS='--color=auto --group-directories-first -hv'

ls="ls $LSOPTS"
alias ll="$ls -l"

alias ls="$ls"
alias lc="$ls --color=never"
alias la="$ls -A"
alias lal="$ls -Al"

alias lsd="$ls -d *(-/N)"       # list visible directories
alias lsf="$ls *(-.N)"          # list visible files

alias lad="$ls -d *(-/DN)"      # list all directories
alias laf="$ls -A *(-.DN)"      # list all files
alias llad="$ls -lhd *(-/DN)"   # list details of all directories
alias llaf="$ls -lhA *(-.DN)"   # list details of all files
alias lld="$ls -lhd *(-/N)"     # list details of visible directories
alias llf="$ls -lh *(-.N)"      # list details of visible files
alias lh="$ls -d .*"            # list hidden files/directories
alias lhd="$ls -d .*(-/N)"      # list hidden directories
alias lhf="$ls .*(-.N)"         # list hidden files
alias llh="$ls -lhd .*"         # list details of hidden files/directories
alias llhd="$ls -lhd .*(-/N)"   # list details of hidden directories
alias llhf="$ls -lh .*(-.N)"    # list details of hidden files

alias le="$ls -d *(-/DN^F)"     # list all empty directories
alias ler="$ls -d **/*(-/DN^F)" # list all empty directories recursively
alias lle="$ls -ld *(-/DN^F)"   # list details of all empty directories
alias ller="$ls -lhd **/*(-/DN^F)" # list details of all empty directories recursively

function chpwd() {
  if [[ -z "$ZSH_NO_CHPWD" ]]; then
    ls
  fi
}

# }}}
# mount / sshfs {{{

alias mmu='mount /mnt/usb'
alias muu='umount /mnt/usb'

alias mmw='sshfs nl:/warez ~/ssh/warez -o allow_other'
alias muw='fusermount -u ~/ssh/warez'
alias mmn='sshfs nl: ~/ssh/ninjaloot'
alias mun='fusermount -u ~/ssh/ninjaloot'

# }}}
# mplayer {{{

alias mplayer="mplayer -msgcolor -msgmodule"

# }}}
# mysql {{{

function my() {
  service=${1:-help}
  shift

  date=$(print -P '%D{%Y.%m.%d}')

  # TODO: Drop and recreate

  case $service in
    dump)
      # TODO: Query for db total size and use pv for visualization?
      mysqldump ${1:--all-databases} > ${1:-full}.$date.mysql

      return $?
    ;;

    load)
      if [[ -z "$1" ]]; then
        zerror "mysqldump filename required"
        return 1
      fi

      print -P "Loading %B${c[23]}${1}%f%b"

      if has pv; then
        pv -pte $1 | mysql
      else
        mysql < $1
      fi
      return $?
    ;;

    remote)
      ssh ${1:-dt} -t mysql $2
      return $?
    ;;

    remote_load)
      host=${1:-dt}
      db=${2:-mancx_django}

      f="$host.$db.$date.mysql"

      # TODO: See TODO on dump()
      if [[ ! -f $f ]]; then
        print -P "Grabbing %B${c[19]}${db}%f%b from %B${c[27]}${host}%f%b"
        ssh $host mysqldump $db > $f || return 1
      fi

      # Use self!
      my load $f
      return $?
    ;;

    size)
      mb="ROUND(((data_length + index_length) / (1024*1024)),2) AS MB"
      q="SELECT table_name, $mb FROM information_schema.tables "
      q+="ORDER BY MB desc LIMIT ${2:-10}"

      ssh ${1:-dt} mysql -e ${(qqqq)q} | column -t

      return $?
    ;;

    *)
      echo "helpful help is helpful"
      return 1
    ;;

  esac
}

# }}}
# python {{{

function touch_init() {
  until [[ -z "$1" ]] && [[ -f "${1:-.}/__init__.py" ]]; do
    touch ${1:-.}/__init__.py
    shift &> /dev/null
  done
}
alias ti="touch_init"

# }}}
# xbmc {{{

alias xu="xbmc_update"
function xbmc_update() {
  p=""
  if [[ -n "$1" ]]; then
    p=",/warez/$1"
  fi

  url="http://sexbmc.local:1337/xbmcCmds/xbmcHttp?"
  url+="command=ExecBuiltIn&parameter=XBMC.updatelibrary(video${p})"

  curl --user xbmc:penis $url
}

autoload -Uz xbmc_update

# }}}

# }}}
# Prompt setup {{{

setopt prompt_subst

# Prevent trailing singlequotes to break the command lines! :D
setopt sunkeyboardhack
export KEYBOARD_HACK="'"

# hax0r git prompt setup!
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' unstagedstr "${c[16]}"
zstyle ':vcs_info:*' stagedstr "${c[18]}"

zstyle ':vcs_info:git*:*' get-revision true
zstyle ':vcs_info:git*:*' check-for-changes true

local gitformat="%B${c[14]}%r${c[1]}(%a${c[15]}%u%c%b%m${c[1]}):${c[4]}/%S"
zstyle ':vcs_info:git*' formats $gitformat
zstyle ':vcs_info:git*' actionformats $gitformat

zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash git-action

# Show remote ref name and number of commits ahead-of or behind
function +vi-git-st() {
  local str branch ahead behind remote

  # Store the current branch name
  branch=${hook_com[branch]}

  # Are we on a remote-tracking branch?
  remote=${$(git rev-parse --verify ${branch}@{upstream} \
    --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

  if [[ -n ${remote} ]] ; then
    str=${branch}

    ahead=$(git rev-list ${branch}@{upstream}..HEAD 2>/dev/null | wc -l)
    behind=$(git rev-list HEAD..${branch}@{upstream} 2>/dev/null | wc -l)

    (( $ahead + $behind )) && str+="${c[1]}:"  # Dark colon if any
    (( $ahead )) && str+="${c[15]}+${ahead}%f"  # Ahead
    (( $ahead )) && (( $behind )) && str+="${c[1]}/"  # Dark slash
    (( $behind )) && str+="${c[16]}-${behind}%f"  # Behind
  else
    # Just add a red colon to mark non-tracking branch
    str="${branch}${c[16]}:"
  fi

  hook_com[branch]=$str
}

# Show count of stashed changes
function +vi-git-stash() {
  local -a stashes
  local git_root

  _find_git_root
  if [[ -n "$git_root" ]] && [[ -s $git_root/refs/stash ]] ; then
    stashes=$(git stash list 2>/dev/null | wc -l)
    hook_com[misc]+="${c[1]}:${c[4]}${stashes}st${c[1]}"
  fi
}

# Sexy hook to get purdy action messages
function +vi-git-action() {
  local s="${hook_com[action]}"
  if [[ -n "$s" ]] ; then
    hook_com[action]="${c[6]}omg ${s}!${c[1]}:"
  fi
}

repo="%B${c[14]}%r${c[1]}" # Repo root
repo+="(${c[15]}%u%b${c[1]})" # Branch and status
repo+=":${c[4]}/%S" # Actual location

zstyle ':vcs_info:*' formats "${repo}"

function p() {
  export PMODE=$1
}

function precmd() {
  if [[ $TERM != "linux" ]]; then
    # Print xterm title
    print -Pn "\e]0;%n@%m: %~\a"
  fi

  if [[ $PMODE -ge 2 ]]; then
    local u="%n@%m"
    local t=""
    if [[ -n "$TMOUT" ]] && [[ "$TMOUT" != 0 ]]; then
      local t="${c[12]}${TMOUT}s${c[1]}-" # Timeouting
    fi

    local e="%(?..${c[6]}$(echo -en "\007")%?${c[1]}-)" # Errors
    local j="%(1j.${c[7]}%jbg${c[1]}-.)" # Jobs

    local r1="%B%(#.${c[3]}%m.${c[2]}$u) ${c[4]}"

    vcs_info
    if [[ -n "${vcs_info_msg_0_}" ]]; then
      # Find the git root.
      local cur=$PWD
      until [[ -z "$cur" ]]; do
        # -r finds dirs and files. new-style submods are files.
        if [[ -r "$cur/.git" ]]; then
          break
        fi
        cur=${cur%/*}
      done

      local p=${${cur/$HOME/\~}:h}
      local git_root
      _find_git_root

      # If we are in a submodule, decorate the super repo
      if [[ "$cur" != "$git_root:h" ]]; then
        local super=${${git_root%/.git/*}##*/}
        p=${p/$super/${c[28]}${super}${c[4]}}
      fi

      # Print in directory blue up until the repo.
      r1+="$p/${vcs_info_msg_0_}"
    else
      r1+="%~"
    fi

    local r2="${t}${j}${e}${c[1]}%B>%b%f "
  fi

  case $PMODE in
    0) ; PROMPT="%# "; ;;
    1) ; PROMPT="%B%(#.${c[3]}%m.${c[2]}%n@%m) ${c[4]}%~ %#%b%f " ; ;;
    *) ; PROMPT=$(print "$r1\n$r2") ; ;;
  esac
}

# }}}
# User specific configuration {{{

local USERFILE="$ZSHCONFDIR/local.zsh"
if [[ -f $USERFILE ]] || [[ -L $USERFILE ]] ; then
  source $USERFILE
fi

# }}}

# vim: ft=zsh fdm=marker fmr={{{,}}}
