#!/bin/zsh

if [[ -z "$1" ]] ; then
  zerror "No files given."
  return
fi

for f in $* ; do
  if [[ -f $f ]]; then
    case $f in
      *.tar.bz2) command tar xjf $f  ;;
      *.tar.gz)  command tar xzf $f  ;;
      *.bz2)     command bunzip2 $f  ;;
      *.rar)     command unrar x $f  ;;
      *.gz)      command gunzip $f   ;;
      *.tar)     command tar xf $f   ;;
      *.tbz2)    command tar xjf $f  ;;
      *.tgz)     command tar xzf $f  ;;
      *.zip)     command unzip $f    ;;
      *.Z)       command uncompress $f ;;
      *.7z)      command 7z x $f     ;;
      *.xz)      command unxz -vk $f   ;;
      *.lzma)    command unlzma -vk $f ;;
      *)     print "'$f' cannot be extracted via x()" ;;
    esac
  else
    print "'$f' is not a valid file"
  fi
done
