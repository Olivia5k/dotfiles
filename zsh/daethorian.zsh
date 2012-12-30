# zshrc local user configuration by Lowe Thiderman (daethorian@ninjaloot.se)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).

# User settings {{{
    # Your main alias. If $USER is the same as $ALIAS, the prompt will save
    # space and only show the hostname instead of both the username and the
    # hostname.
    export ALIAS='daethorian'

    # Your full name. Used with git configurations.
    export FULLNAME="Lowe Thiderman"

    # Your email adress. Used with git configurations.
    export EMAIL="lowe.thiderman@gmail.com"

    export PMODE=4

    # Make root timeout after 90 seconds for security reasons.
    # Unset (and/or comment out here) to disable.
    export ROOT_TIMEOUT=90
    export CONFIG="$HOME/config"
    export EDITOR="vim"
    export VISUAL="vim"
    export MAIL="$HOME/var/mail"

    export ZCOLOR="default"
    source $ZSHCONFDIR/colorschemes/$ZCOLOR.zsh

    # XDG misses a log specification :(
    export LOGS="$HOME/.local/logs"

    HISTFILE="$LOGS/zsh.history.log"
    HISTSIZE=100000
    SAVEHIST=100000

    export HOMENET="ninjanet"

    export HOMEBIN="$HOME/.local/bin"
    local _PATH="$HOME/bin:$HOMEBIN"

    # If $_PATH is not in $PATH, add it, but only once.
    if ! [[ $PATH =~ "$_PATH" ]] ; then
        export PATH=$_PATH:$PATH
    fi

    # It is not uncommon to always supply some arguments to common commands. ls
    # and grep needs colors, right?
    export LSOPTS='--color=auto --group-directories-first -hv'
    export GREPOPTS='--color=auto'
# }}}
# User zsh specifics {{{
    export ZDUMPDIR=

    # The globbing!
    setopt extendedglob
    umask 022
# }}}
# Modules {{{
    # zsh module directory
    export ZMODDIR="$ZSHCONFDIR/modules"

    # Core modules are recommended and should most probably always be loaded.
    for i in $ZMODDIR/*.zsh(n) ; do
        _modload $i
    done

    # Shell syntax highlighting, in realtime.
    if [[ "$TCOLORS" = 256 ]]; then
        source $ZMODDIR/ext/zsh-syntax-highlighting-filetypes/zsh-syntax-highlighting-filetypes.zsh
    fi

    # Application specific modules; loaded if they are installed
    for m in $ZMODDIR/apps/* ; do
        app=${${m##*/}%\.*}  # Strip down to the actual executable name
        # if has $app ; then
            _modload "apps/$app"
        # fi
    done
    unset m
# }}}
# User custom whatever {{{
    # Put whatever else you want here that is specific to your setup.
    if has mpc; then
        export MPD_HOST=localhost
        export MPD_PORT=6600
    fi

    if [[ "$UID" != 0 ]]; then
        fpath=( $ZSHCONFDIR/completion "${fpath[@]}" )
    fi

    alias wpg="touch /tmp/gemma && wp"
    alias wpn="rm /tmp/gemma &> /dev/null && wp"

    # Project directories
    alias cdg='cd ~/git'
    # alias cdm='cd ~/git/django-mancx'
    alias cdp='cd /usr/lib/python2.7/site-packages/'

    # USB, yeah
    alias mmu='mount /mnt/usb'
    alias muu='umount /mnt/usb'

    # Warez
    alias mmw='sshfs nl:/warez ~/ssh/warez -o allow_other'
    alias muw='fusermount -u ~/ssh/warez'

    # sshfs home
    alias mmn='sshfs nl: ~/ssh/ninjaloot'
    alias mun='fusermount -u ~/ssh/ninjaloot'

    # Force unmounting
    alias muf='sudo umount -l /mnt/warez && sudo umount -l ~/ssh/ninjaloot'

    # Selenium Xvfb setup
    alias selenium_xvfb="Xvfb :99 -ac -screen 0 1024x768x8"
    alias sx="selenium_xvfb"

    # XBMC
    alias xu="xbmc_update"
    xbmc_update() {
        p=""
        if [[ -n "$1" ]]; then
            p=",/mnt/warez/$1"
        fi

        url="http://sexbmc.local:1337/xbmcCmds/xbmcHttp?"
        url+="command=ExecBuiltIn&parameter=XBMC.updatelibrary(video${p})"

        curl --user xbmc:penis $url
    }

    autoload -Uz xbmc_update

    # For use of libs you compiled yourself.
    export LD_LIBRARY_PATH="/usr/lib:/usr/local/lib"

    function dump() {
        if [[ -z "$1"  ]]; then
            zerror "fielnaem pls"
            return 1
        fi

        if [[ ! -f "$1" ]]; then
            zerror "$1: no such file"
            return 127
        fi

        d="/srv/dump"
        url="http://dump.ninjaloot.se"
        if [[ -f "$d/$1" ]]; then
            zerror "$1 already exists at $url/$1."
            return 2
        fi

        cp $1 $d
        echo $url/$1
        if [[ -n "$DISPLAY" ]]; then
            echo $url/$1 | xclip
        fi
    }
# }}}

# vim: ft=zsh fmr={{{,}}}
