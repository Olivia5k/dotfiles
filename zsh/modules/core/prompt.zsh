# CVS integration
setopt prompt_subst
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr "%f%F{${c[16]}}" # Just make it red!

#zstyle ':vcs_info:*' actionformats "${repo}─[%F{${c[17]}}%a%F{${c[1]}}]${branch}"
#zstyle ':vcs_info:(svn):*' branchformat '%b'

function p()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Change prompt behaviour."
        fi
        return
    fi

    if [[ "$1" = "3" ]] ; then
        export PKEEP=false
    else
        export PKEEP=true
    fi

    export PMODE=$1
}

# Since the two functions below are potentially executed on every prompt
# reload, they do not have any inboud documentation.

function precmd()
{
    if [[ $TERM != "linux" ]] ; then
        # Print xterm title
        print -Pn "\e]0;%n@%m: %~\a"

        # Add svn?
        if [[ $PWD =~ "git/" ]] || [[ -d "$PWD/.git" ]] ; then
            if ! $PKEEP && [[ $PMODE != 3 ]] ; then
                export POLD=$PMODE
                export PMODE=4
            fi
        fi
    fi

    if [[ $PMODE -ge 3 ]] ; then
        if [[ $PMODE = 4 ]]; then
            repo="%B%F{${c[14]}}%r%F{${c[1]}}(%F{${c[15]}}%u%b%F{${c[1]}}):%F{${c[4]}}/%S"
            branch=""
        elif [[ $PMODE = 5 ]] ; then
            repo="%B%F{${c[14]}}%r%F{${c[1]}}:%F{${c[4]}}/%S"
            branch=" %F{${c[15]}}%u%b%%b"
        else
            repo="%B%F{${c[1]}}├─[%F{${c[13]}}%s%F{${c[1]}}:%F{${c[14]}}%r%F{${c[1]}}]"
            branch="─[%F{${c[15]}}%u%b%F{${c[1]}}]%%b"
        fi

        zstyle ':vcs_info:*' formats "${repo}${branch}"
        vcs_info

        # If the message is empty, we are no longer in the CVS.
        if [[ -z "${vcs_info_msg_0_}" ]] ; then
            export PMODE=$POLD
        fi
    fi
    prompt
}

function prompt()
{
    if [[ $PMODE -ge 2 ]] ; then
        if [[ "$USER" != "$ALIAS" ]] ; then
            local u="%n@%m"
        else
            local u="%m"
        fi
        if $HASMULTI && [[ $TERM != $MULTITERM ]] ; then
            local dc=${c[5]}
        else
            local dc=${c[4]}
        fi

        if [[ -d $MAIL ]] ; then
            local mc=$(find $MAIL | grep new/ | wc -l)
            if [[ $mc -gt 0 ]] ; then
                local m="─[%F{${c[9]}}${mc}M%F{${c[1]}}]"
            fi
        fi

        local t=""
        if [[ -n "$TMOUT" ]] && [[ $TMOUT != 0 ]] ; then
            local t="─[%F{${c[12]}}${TMOUT}%F{${c[1]}}]"
        fi

        local b=''
        # Kernel battery info, with primitive caching! \o/
        if false && $LAPTOP && [[ -f $BAT ]] ; then
            if [[ -f $BATC ]] && [[ $(date +'%s') -le $BATT ]]; then
                local bat=$(cat $BATC)
            else
                local bat=$(bat)
                echo $bat > $BATC

                # 60 second cache
                export BATT=$(($(date +'%s') + $BATS))
            fi
            local p=$(echo $bat | cut -f1 -d\|)
            local t=$(echo $bat | cut -f2 -d\|)

            local b="─[%F{${c[10]}}${p}%%%F{${c[1]}}]─[%F{${c[11]}}${t}%F{${c[1]}}]" # Battery
        fi

        local e="%(?..─[%F{${c[6]}}$(bell)%?%F{${c[1]}}])" # Errorcodes
        local j="%(1j.─[%F{${c[7]}}%j%F{${c[1]}}].)" # Jobs

        if [[ $PMODE -ge 4 ]]; then
            if [[ -n "${vcs_info_msg_0_}" ]]; then
                local r1="%B%(#.%F{${c[3]}}%m.%F{${c[2]}}$u) ${vcs_info_msg_0_} "
            else
                local r1="%B%(#.%F{${c[3]}}%m.%F{${c[2]}}$u) %F{${c[4]}}%~ "
            fi
            local r2="%(?..%F{${c[6]}}$(bell)%?)%F{${c[1]}}%B>%b%f "
        else
            local r1="%B%F{${c[1]}}┌─[%(#.%F{${c[3]}}%m.%F{${c[2]}}$u)%F{${c[1]}}]$t─[%F{$dc}%~%F{${c[1]}}]%b"
            local r2="%B%F{${c[1]}}└${e}${b}${j}${m}─[%F{${c[8]}}%D{%H:%M}%F{${c[1]}}]>%f%b "
        fi
    fi

    case $PMODE in
        0) ; PROMPT="%# "; ;;
        1) ; PROMPT="%B%(#.%F{${c[3]}}%m.%F{${c[2]}}%n@%m) %F{${c[4]}}%~ %#%b%f " ; ;;
        2 | 4) ; PROMPT=$(print "$r1\n$r2") ; ;;
        3) ; PROMPT=$(print "$r1\n${vcs_info_msg_0_}\n$r2") ; ;;
    esac
}
