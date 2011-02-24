#!/bin/zsh

setopt extendedglob

if [[ -z "$1" ]] ; then
	tmux display-message inject-window: Window index needed
	exit 0
fi

target=$1
local x=0
local IFS=$'\n'
local active=""
local search=false
local next=""
local start=""
local end=""

for line in $(tmux list-win) ; do
	(( x++ ))
	idx=${line/:(*)#/}

	if [[ "$line" =~ "\(active\)$" ]] ; then
		active=$idx
		if [[ "$active" = "$target" ]] ; then
			exit 0
		fi
	fi

	if [[ "$target" = "$idx" ]] ; then
		search=true
		start=$idx
		#print start $idx
	fi

	# Check if this index is last + 1
	if $search && [[ "$idx" != "$(( last + 1 ))" ]] ; then
		#print end $last
		#mvlist+=$idx
		end=$last
		search=false
	fi
	last=$idx
done

if [[ -z "$end" ]] ; then
	end=$start
fi

span=({$start..$end})
print ${span}

for idx in ${(Oa)span} ; do
	echo tmux move-window -d -s :$idx -t :"$(( idx + 1 ))" &> /dev/null
	tmux move-window -d -s :$idx -t :"$(( idx + 1 ))"
done
echo tmux move-window -s :$active -t :$target &> /dev/null
tmux move-window -s :$active -t :$target
