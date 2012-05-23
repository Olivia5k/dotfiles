setopt prompt_subst

# Prevent trailing singlequotes to break the command lines! :D
setopt sunkeyboardhack
export KEYBOARD_HACK="'"

repo="%B%F{${c[14]}}%r%F{${c[1]}}" # Repo root
repo+="(%F{${c[15]}}%u%b%F{${c[1]}})" # Branch and status
repo+=":%F{${c[4]}}/%S" # Actual location

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
            local t="%F{${c[12]}}${TMOUT}s%F{${c[1]}}-" # Timeouting
        fi

        local e="%(?..%F{${c[6]}}$(echo -en "\007")%?%F{${c[1]}}-)" # Errors
        local j="%(1j.%F{${c[7]}}%jbg%F{${c[1]}}-.)" # Jobs

        local r1="%B%(#.%F{${c[3]}}%m.%F{${c[2]}}$u) %F{${c[4]}}"

        vcs_info
        if [[ -n "${vcs_info_msg_0_}" ]]; then
            # Find the git root.
            cur=$PWD
            until [[ -z "$cur" ]]; do
                if [[ -d "$cur/.git" ]]; then
                    break
                fi
                cur=${cur%/*}
            done

            # Print in directory blue up until the repo.
            r1+="${${cur/$HOME/\~}:h}/${vcs_info_msg_0_}"
        else
            r1+="%~"
        fi

        local r2="${t}${j}${e}%F{${c[1]}}%B>%b%f "
    fi

    case $PMODE in
        0) ; PROMPT="%# "; ;;
        1) ; PROMPT="%B%(#.%F{${c[3]}}%m.%F{${c[2]}}%n@%m) %F{${c[4]}}%~ %#%b%f " ; ;;
        *) ; PROMPT=$(print "$r1\n$r2") ; ;;
    esac
}
