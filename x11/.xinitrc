#!/bin/zsh

eval $(ssh-agent)
exec dunst &
exec xsetroot -cursor_name left_ptr &
exec xset -b &
exec xrdb ~/.Xresources &
exec $HOME/.local/bin/keyboard-setup &
exec $HOME/.local/bin/wp &
exec /etc/X11/xinit/xinitrc.d/50-systemd-user.sh &

export VISUAL=emacsclient
export EDITOR="$VISUAL"

exec emacs
