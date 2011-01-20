alias ia='ifconfig -a'
alias ar='sudo /etc/init.d/apache2 restart'

alias shutdown='sudo shutdown -h now'
alias reboot='sudo reboot'

# grep processes and retain grep color
function psg()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Grep process names."
		fi
		return
	fi

	if [[ -z "$1" ]] ; then
		_zerror "Arguments plx"
		return
	fi

	ps haux | grep -i $1 | grep -Ev '(grep)' | grep -i $1
}

# control daemons: d <daemon> <action>
function d()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Daemon control and listing. Has completion."
		fi
		return
	fi

	if [[ -z "$1" ]] ; then
		p="/var/run/daemons"
		for f in $(ls /etc/rc.d/*(.x)) ; do
			d=${f##*/}
			if [[ -f "$p/$d" ]] ; then
				c="green"
			else
				c="240" # \o/
			fi
			print -P "%B%F{$c}${d}%f%b"
		done
		return
	fi

	if [[ "$UID" = 0 ]] ; then
		/etc/rc.d/$1 $2
	else
		sudo /etc/rc.d/$1 $2
	fi
}

_daemoncomplete()
{
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
compctl -Y "%B%F{red}daemon%f%b" -K _daemoncomplete d

# Control wireless network
function nr()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "netcfg wireless management"
		fi
		return
	fi

	if [[ -n "$1" ]] ; then
		wlan=$1
	else
		wlan=$HOMENET
	fi

	sudo netcfg -d $wlan &> /dev/null
	sudo netcfg $wlan
}

# Print IP address info.
function ips()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "IP adress information printer (v4 and v6)"
		fi
		return
	fi

	for i in $(ifconfig -a | grep -Eo "^[a-z0-9]+"); do
		ifdata=$(ifconfig $i)

		ip=$(echo $ifdata | grep -Eo "inet addr:[0-9.]* " | grep -Eo "([0-9]+\.?){4}")
		ip6=$(echo $ifdata | grep -Eo "inet6 addr: [a-f0-9:/]* " | grep -Eo "[a-f0-9:/]+ $")

		if [[ -n "$ip" ]] ; then
			s="%F{green}%B${i}%b%f: %F{cyan}%B${ip}%b%f"
			if [[ -n "$ip6" ]] ; then
				s+=" and %F{cyan}%B${ip6}%b%f"
			fi
			print -P $s
		else
			print -P "%F{red}%B${i}%b%f"
		fi
	done

	# Thank you Loopia! \o/
	pub=$(curl http://dns.loopia.se/checkip/checkip.php 2> /dev/null | grep -Eo "([0-9]+\.?){4}")
	
	if [[ -n "$pub" ]] ; then
		print -P "%F{green}%BPublic%b%f: %F{cyan}%B${pub}%b%f"
	else
		print -P "%F{red}%BPublic%b%f"
	fi

	avahi=$(ps aux | grep "avahi-daemon: running" | grep -Eo "\[.*\]" | tr -d "[]")
	if [[ -n "$avahi" ]] ; then
		print -P "%F{green}%BAvahi%b%f: %F{cyan}%B${avahi}%b%f"
	fi
}
