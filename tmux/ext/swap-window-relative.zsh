#!/bin/zsh

setopt extendedglob

# It seems that if these two are local. IFS gets overridden. :S
# TODO: Report bug?
win=()
local x=0
local IFS=$'\n'
local active=""
local target=""

for line in $(tmux list-win) ; do
	(( x++ ))
	win+=${line/:(*)#/}

	if [[ "$line" =~ "\(active\)$" ]] ; then
		active=$x
	fi
done

if [[ "$1" = "-U" ]] ; then
	if [[ "$active" = "$x" ]] ; then
		# Last window. Goto first.
		target=1
	else
		# Increase
		target=$(( active + 1 ))
	fi
else
	if [[ "$active" = ${win[1]} ]] ; then
		# First window. Goto last.
		target=$x
	else
		# Decrease
		target=$(( active - 1 ))
	fi
fi

tmux swap-window -s :${win[$active]} -t :${win[$target]}
unset win
