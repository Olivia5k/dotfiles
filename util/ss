#!/bin/zsh

# Lol screenshooter with dzen2-xft reporting
# Uploads to local server and feeds URL to clipboard. Whatup.
# Written by Lowe Thiderman (daethorian@gmail.com)
# Licensed under the WTFPL.

remote=true
if [[ $HOST = "ninjaloot" ]] ; then
    remote=false
fi

date=$(date +'%Y%m%d.%H%M%S')
user="daethorian"
server="ninjaloot.se"
localpath="$HOME/var/img/ss/dated"
remotepath="var/http/ninjaloot/ss/"
url="http://shot.$server/$date.png"
msg_timeout=3

w=190
fgc="#007b8c"
bgc="#1a1a1a"
e="onstart=uncollapse"
font="xft:inconsolata:bold:pixelsize=14"

# If called by automatic screenshooter, change some parameters
if [[ "$1" = "auto" ]]; then
    localpath+="/auto"
    remotepath="var/img/ss/auto"
    fgc="#6c71c4"
else
    # Only send URL if not auto; it will clutter the clipboard otherwise
    echo -n $url | xclip
fi

scrot $localpath/$date.png

if $remote; then
    scp $localpath/$date.png $user@$server:$remotepath #&> /dev/null
else
    cp $localpath/$date.png $remotepath
fi

for x in {1..$msg_timeout} ; do
    echo " ^fg($fgc)$date.png"
    sleep 1
done | dzen2 -fn $font -sa l -bg $bgc -x -$w -y 19 -w $w -e $e
