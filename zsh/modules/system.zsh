alias ar='sudo /etc/init.d/apache2 restart'
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
            print -P "%B%F{$co}${d}%f%b"
        done
        return
    fi

    if [[ "$UID" = 0 ]] ; then
        /etc/rc.d/$1 $2
    else
        sudo /etc/rc.d/$1 $2
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
    if [[ -n "$1" ]] ; then
        wlan=$1
    else
        wlan=$HOMENET
    fi

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

# https://github.com/sickill/stderred
if [ -f "/usr/lib/stderred.so" ]; then
    export LD_PRELOAD="/usr/lib/stderred.so"
fi
