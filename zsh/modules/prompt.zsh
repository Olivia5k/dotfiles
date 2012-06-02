setopt prompt_subst

# Prevent trailing singlequotes to break the command lines! :D
setopt sunkeyboardhack
export KEYBOARD_HACK="'"

# hax0r git prompt setup!
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' unstagedstr "${c[16]}"
zstyle ':vcs_info:*' stagedstr "${c[18]}"

zstyle ':vcs_info:git*:*' get-revision true
zstyle ':vcs_info:git*:*' check-for-changes true

f="%B${c[14]}%r${c[1]}(%a${c[15]}%u%c%b%m${c[1]}):${c[4]}/%S"
zstyle ':vcs_info:git*' formats $f
zstyle ':vcs_info:git*' actionformats $f

zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash git-action

# Show remote ref name and number of commits ahead-of or behind
function +vi-git-st() {
    local str branch ahead behind remote

    # Store the current branch name
    branch=${hook_com[branch]}

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${branch}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} ]] ; then
        str=${branch}

        ahead=$(git rev-list ${branch}@{upstream}..HEAD 2>/dev/null | wc -l)
        behind=$(git rev-list HEAD..${branch}@{upstream} 2>/dev/null | wc -l)

        (( $ahead + $behind )) && str+="${c[1]}:"  # Dark colon if any
        (( $ahead )) && str+="${c[15]}+${ahead}%f"  # Ahead
        (( $ahead )) && (( $behind )) && str+="${c[1]}/"  # Dark slash
        (( $behind )) && str+="${c[16]}-${behind}%f"  # Behind
    else
        # Just add a red colon to mark non-tracking branch
        str="${branch}${c[16]}:"
    fi

    hook_com[branch]=$str
}

# Show count of stashed changes
function +vi-git-stash() {
    local -a stashes

    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l)
        hook_com[misc]+="${c[1]}:${c[4]}${stashes}st${c[1]}"
    fi
}

# Sexy hook to get purdy action messages
function +vi-git-action() {
    local s="${hook_com[action]}"
    if [[ -n "$s" ]] ; then
        hook_com[action]="${c[6]}omg ${s}!${c[1]}:"
    fi
}

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
