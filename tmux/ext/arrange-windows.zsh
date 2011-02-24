#!/bin/zsh

setopt extendedglob

if [[ -n "$1" ]] ; then
	tmux display-message inject-window: Window index needed
	exit 0
fi

win=()
x=0
IFS=$'\n'
active=""
last=""
start=""

for line in $(tmux list-win) ; do
	idx=${line/:(*)#/}

	if [[ "$line" =~ "\(active\)$" ]] ; then
		active=$idx
	fi

	if [[ "$(( x++ ))" != "$idx" ]] ; then
		if [[ -z "$start" ]] ; then
			start=$idx
		fi
		win+=$idx
		print move $idx
	else
		print stay $idx
	fi
	(( x++ ))
done

for i in $(seq 1 3); do
	print hehe
	(( x++ ))
	print tmux move-window -d -s :$i -t :$y #&> /dev/null
	#tmux move-window -d -s :$idx -t :"$(( idx + 1 ))"
done
