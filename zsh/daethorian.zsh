# zshrc local user configuration by Lowe Thiderman (daethorian@ninjaloot.se)
# Released under the WTFPL (http://sam.zoy.org/wtfpl/).
#
# <github link>

# This is a file containing user specific configuration for zsh. While the
# original intent is for .zshrc to contain all this, it becomes troublesome
# whenever you want to share your zshrc and those you share it with make
# changes.

# The best way to use this file is to copy it to ~/config/zsh/$USER.conf and
# mofify it to your needs. Whenever the zsh is updated (from git ;)), your
# changes will be intact and you can diff properly without conflicts. Sweet.
# $USER.conf files will be sourced from the main zshrc in favor of this file.

# Within this file there are several boolean togglers. If not stated otherwise,
# they need to be set _and_ have a value of true. This way both commenting them
# out and setting them to false will have immediate effect.

# User settings {{{
    # User information {{{
        # Your main alias. If $USER is the same as $ALIAS, the prompt will save
        # space and only show the hostname instead of both the username and the
        # hostname.
        export ALIAS='daethorian'

        # Your main remote site. Currently not widely used. Backup and connection
        # testing functions are planned.
        export REMOTE='ninjaloot.se'

        # Your full name. Used with git configurations.
        export FULLNAME="Lowe Thiderman"

        # Your email adress. Used with git configurations.
        export EMAIL="${ALIAS}@$REMOTE"
    # }}}
    # User prompt settings {{{
        # Your prompt mode. Currently four modes are supported:
        # #0: A simple % (user) or # (root). No more, no less. Used for minimalism.
        # #1: $USER@$HOST $PWD. Identical to Gentoos basic bash prompt. Used for consoles.
        # Will not expand back to only $HOST if $USER and $ALIAS match.
        #
        # #2: The default prompt. Uses double rows and is pretty advanced.
        # Always shows current machine, current PWD and a clock.
        # It also has several situational modules:
        #     # Exitstatus. Shown whenever exitstatus is non-zero.
        #     # Jobs. Shown whenever you have backgrounded jobs.
        #     # Mail. Shown whenever there are files in any maildir in $MAIL.
        #     # Battery. Shown if laptop and if battery is toggled.
        #
        # Mail and battery requires simple setting up and are ignored otherwise.
        #
        # #3: The CVS prompt. Identical to #2, but inserts CVS info in a middle row.
        # It is automatically toggled whenever you enter a directory that contains
        # CVS files, and automatically reverted whenever you leave.
        #
        # You can toggle between the prompts using the simple p() shell function
        # using numerical arguments. If you give a boolean argument (true/false),
        # the prompt will set $PKEEP. If $PKEEP is true, the prompt will not
        # try to automatically switch to the CVS prompt.
        export PMODE=4
        export PKEEP=false

        # Force the console to use prompt #1
        export FORCE_CONSOLE=true

        # Force mobile connections (any $TERM that is 'xterm') to use prompt #0.
        export FORCE_MOBILE=false

        # Make root timeout after 180 seconds for security reasons.
        # Unset (and/or comment out here) to disable.
        export ROOT_TIMEOUT=180
    # }}}
    # User configurations {{{
        export CONFIG="$HOME/config"
    # }}}
    # User editor {{{
        # Your editor. If an alias exists for it, it will be unaliased to avoid
        # confusion and breakage. (if one exists and you run which on it, which will
        # not return an absolute path to the application)
        local _EDITOR='vim'

        if alias $_EDITOR &> /dev/null ; then
            unalias $_EDITOR
        fi

        if which $_EDITOR &> /dev/null ; then
            export EDITOR=$(which $_EDITOR)
        else
            export EDITOR=$(which vi)
        fi

        # Your visual editor. Nuff said.
        export VISUAL=$EDITOR
        alias e=$EDITOR
    # }}}
    # User colorscheme {{{
        if [[ $TCOLORS = 256 ]]; then
            export ZCOLOR="frostbite"
        else
            export ZCOLOR="default"
        fi
    # }}}
    # User directories and logs {{{
        # Your mail directory. If set and exists, the prompt will look for new mail
        # in maildirs within it. The principle is simple and very primitive, so any
        # file within a directory named new/ inside $MAIL will trigger the mail
        # count.
        # Note that unlike most directories bound to the zsh conf, $MAIL will not be
        # created automatically.
        export MAIL="$HOME/mail"

        # Your log directory. zsh will place it's history there instead of
        # cluttering the $HOME.
        # XDG misses a log specification :(
        export LOGS="$HOME/.local/logs"

        HISTFILE="$LOGS/zsh.history.log"
        HISTSIZE=100000
        SAVEHIST=100000
    # }}}
    # User laptop settings {{{
        # Set if laptop. If false, no battery settings will take place.
        export LAPTOP=true

        # Battery settings.
        # If the file $BAT exists, the prompt triggers the battery module.
        # When triggered, it writes a simple cache to $BATC and uses this for $BATS
        # seconds. $BATT is the timestamp of when the cache was last reset.
        # The cache is mainly to not access battery files every time a prompt is
        # read.
        if [[ -n "$LAPTOP" ]] && $LAPTOP ; then
            export BAT='/tmp/battery'
            export BATC='/tmp/battery_cache' # Battery cache
            export BATT=0 # Battery timeout
            export BATS=30 # Battery shift
        fi

        # Your home network name.
        export HOMENET="ninjanet"
    # }}}
    # User multiplexer {{{
        # Your terminal multiplexer. If installed and you are not currently in it
        # (whether you are or not is decided if $TERM is equal to $MULTITERM) the
        # PWD in the prompt will be red. Unset $MULTI to disable.
        export MULTI='tmux'
        export MULTITERM='screen-256color'
    # }}}
    # User chpwd and path {{{
        # Your todo list. The defaults are specified for devtodo
        # (http://swapoff.org/DevTodo). If $TODO is installed and $TODOFILE is in
        # the current directory, $TODO is run upon chpwd().
        export TODO='todo'
        export TODOFILE='.todo'

        # When I used lscmd() (included in main zshrc) and found out that 25% of all
        # the commands I ever used in zsh was ls, I figured that it could be more
        # effective and put ls into chpwd.
        # It takes some getting used to, but it is really worth it!
        export CHPWD=true

        # Your choice of chpwd() command.
        _chpwd() {
            ls
        }

        # Your home bin. In the rc there are functions included that handles
        # installation and uninstallation of custom executables. If this is desired,
        # $HOMEBIN must of course be included in your path.
        export HOMEBIN="$HOME/.local/bin"

        # Your path. Remember to separate additional directories with a colon.
        local _PATH="$HOME/bin:$HOMEBIN"

        # If $_PATH is not in $PATH, add it, but only once.
        if ! [[ $PATH =~ "$_PATH" ]] ; then
            export PATH=$_PATH:$PATH
        fi
    # }}}
    # User coreutils options {{{
        # It is not uncommon to always supply some arguments to common commands. ls
        # and grep needs colors, right?
        export LSOPTS='--color=auto --group-directories-first'
        export GREPOPTS='--color=auto'
    # }}}
    # User zsh specifics {{{
        # zsh specific directory that the core shell might use for dumping etc. Only
        # used when set.
        export ZDUMPDIR="$XDG_DATA_HOME/zsh"

        # The completion system uses a cache file to speed up completion. To avoid
        # cluttering the $HOME, it is put inside $ZDUMPDIR
        export COMPDUMP="$ZDUMPDIR/compdump"

        # Use the debugger?
        export DEBUG=false

        # The globbing!
        setopt extendedglob
        umask 022

        # While vim is superior, shells in vi mode are unfortunately not.
        bindkey -e
    # }}}
# }}}
# Modules {{{
    # zsh module directory
    export ZMODDIR="$HOME/config/zsh/modules"

    # Set the loaded module array
    ZMODULES=()
    export ZMODULES

    # Core modules are recommended and should most probably always be loaded.
    for i in $ZMODDIR/core/* ; do
        _modload $i
    done

    # Chpwd. If you want commands executed on cd, this is the way to go.
    _modload "chpwd"

    # Colorscheme printers. Only useful if you customize alot.
    _modload "colors"

    # Configuration specific aliases
    _modload "conf"

    # Failsafe aliases that catches common misspelled commands and runs the
    # original ones. Also, it is incredibly angry.
    _modload "failsafe"

    # Management of local home path
    _modload "install"

    # LOLCODE!! Mostly useless but kinda lolz.
    _modload "lolcode"

    # Mounting aliases.
    _modload "mount"

    # Application specific modules; loaded if they are installed
    for m in $ZMODDIR/apps/* ; do
        app=${${m##*/}%\.*}  # Strip down to the actual executable name
        _zdebug "Testing for $app"
        if _has $app ; then
            _modload "apps/$app"
        fi
    done
    unset m
# }}}

# User custom whatever {{{
    # Put whatever else you want here that is specific to your setup.
    export PYLINTRC="$HOME/.config/pylint/pylintrc"
    export DJANGO_SETTINGS_MODULE="settings"
    export MAILCHECK=0
    alias ms="rsync $REMOTE:mail/ $MAIL -a --delete &> /dev/null"
    alias mplayer="mplayer -msgcolor -msgmodule"

    alias wpg="touch /tmp/gemma && wp"
    alias wpn="rm /tmp/gemma &> /dev/null && wp"

    # Project directories
    #alias cdc='cd ~/config'
    alias cdg='cd ~/git'
    alias cdm='cd ~/git/django-mancx'
    alias cdu='cd /warez/unpack' # onoes
    alias cdp='cd /usr/lib/python2.7/site-packages/'

    lscf=$HOME/config/zsh/LS_COLORS/LS_COLORS
    if [[ -f $lscf ]] ; then
        eval $(dircolors -b $lscf)
    fi

    source ~/git/ext/trapd00r/configs/zsh/10-*
#}}}

# vim: ft=zsh fmr={{{,}}}
