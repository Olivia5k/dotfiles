setopt prompt_subst

# Prevent trailing singlequotes to break the command lines! :D
setopt sunkeyboardhack
export KEYBOARD_HACK="'"

repo="%B${c[14]}%r${c[1]}" # Repo root
repo+="(${c[15]}%u%b${c[1]})" # Branch and status
repo+=":${c[4]}/%S" # Actual location

zstyle ':vcs_info:*' formats "${repo}"

function p() {
    export PMODE=$1
}

function precmd() {
    if [[ $TERM != "linux" ]]; then
        # Print xterm title
        print -Pn "\e]0;%n@%m: %~\a"
    fi

    if [[ $PMODE -ge 2 ]]; then
        local u="%m"
        if [[ "$USER" != "$ALIAS" ]]; then
            u="%n@%m"
        fi

        local t=""
        if [[ -n "$TMOUT" ]] && [[ "$TMOUT" != 0 ]]; then
            local t="${c[12]}${TMOUT}s${c[1]}-" # Timeouting
        fi

        local e="%(?..${c[6]}$(echo -en "\007")%?${c[1]}-)" # Errors
        local j="%(1j.${c[7]}%jbg${c[1]}-.)" # Jobs

        local r1="%B%(#.${c[3]}%m.${c[2]}$u) ${c[4]}"

        vcs_info
        if [[ -n "${vcs_info_msg_0_}" ]]; then
            # Find the git root.
            cur=$PWD
            until [[ -z "$cur" ]]; do
                # -r finds dirs and files. new-style submods are files.
                if [[ -r "$cur/.git" ]]; then
                    break
                fi
                cur=${cur%/*}
            done

            # Print in directory blue up until the repo.
            r1+="${${cur/$HOME/\~}:h}/${vcs_info_msg_0_}"
        else
            r1+="%~"
        fi

        local r2="${t}${j}${e}${c[1]}%B>%b%f "
    fi

    case $PMODE in
        0) ; PROMPT="%# "; ;;
        1) ; PROMPT="%B%(#.${c[3]}%m.${c[2]}%n@%m) ${c[4]}%~ %#%b%f " ; ;;
        *) ; PROMPT=$(print "$r1\n$r2") ; ;;
    esac
}
