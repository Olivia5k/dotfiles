#/bin/bash

# Author: Jurica Bradaric <jbradaric at gmail.com> 
# A simple script to use custom stylesheets for websites.
# Similar to the Stylish extension for Firefox.
# I hope someone finds it useful. All comments and suggestions
# are welcome.

# Simplified by daethorian (daethorian@ninjaloot.se)

XDG_DATA_HOME=${XDG_DATA_HOME:-"$HOME/.local/share/"}

# Set the path to the .css files
# Every custom css style must be in a file named web_address.css
# e.g. reddit.com.css

STYLE_PATH=$XDG_DATA_HOME/uzbl/css

#echo -e "\n$1 \n$2 \n$3 \n$4 \n$5 \n$6 \n$7 \n$8" >> $HOME/.awesome.log

for i in `ls $STYLE_PATH/*.css`; do
	stylesheet=`basename $i '.css'`
	if [[ "$6" =~ "${stylesheet}" ]]; then
		echo "set stylesheet_uri = file://${STYLE_PATH}/${stylesheet}.css" | socat - unix-connect:$5
		echo "set stylesheet_uri = file://${STYLE_PATH}/${stylesheet}.css" >> $HOME/.awesome.log
		break
	fi
done

exit 0
