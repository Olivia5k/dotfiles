#!/bin/zsh

eval $(ssh-agent)
exec dunst &
exec xsetroot -cursor_name left_ptr &
exec xset -b &
exec xrdb ~/.Xresources &
exec $HOME/.local/bin/keyboard-setup &
exec $HOME/.local/bin/wp &

export VISUAL=emacsclient
export EDITOR="$VISUAL"

exec emacs --debug-init
