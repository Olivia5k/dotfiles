export HASCVS=true

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' unstagedstr "%F{${c[16]}}"
zstyle ':vcs_info:*' stagedstr "%F{${c[18]}}"

zstyle ':vcs_info:git*:*' get-revision true
zstyle ':vcs_info:git*:*' check-for-changes true

f="%B%F{${c[14]}}%r%F{${c[1]}}(%a%F{${c[15]}}%u%c%b%m%F{${c[1]}}):%F{${c[4]}}/%S"
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

        (( $ahead + $behind )) && str+="%F{${c[1]}}:"  # Dark colon if any
        (( $ahead )) && str+="%F{${c[15]}}+${ahead}%f"  # Ahead
        (( $ahead )) && (( $behind )) && str+="%F{${c[1]}}/"  # Dark slash
        (( $behind )) && str+="%F{${c[16]}}-${behind}%f"  # Behind
    else
        # Just add a red colon to mark non-tracking branch
        str="${branch}%F{${c[16]}}:"
    fi

    hook_com[branch]=$str
}

# Show count of stashed changes
function +vi-git-stash() {
    local -a stashes

    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l)
        hook_com[misc]+="%F{${c[1]}}:%F{${c[4]}}${stashes}st%F{${c[1]}}"
    fi
}

# Sexy hook to get purdy action messages
function +vi-git-action() {
    local s="${hook_com[action]}"
    if [[ -n "$s" ]] ; then
        hook_com[action]="%F{${c[6]}}omg ${s}!%F{${c[1]}}:"
    fi
}

# Committing / General
alias ga='git add'
alias gs='git status'
alias gss='git status --short'
alias gc='git commit'
alias gca='gc --all'
alias gp='git push'
alias gu='git pull'
alias gpp='git push origin'
alias guu='git pull origin'

# Branching (only really useful with -v, really)
alias gb='git branch -v'
alias gba='git branch -av'
alias gbv='git branch -vv'
alias gbav='git branch -avv'

# Remotes
alias gre='git remote -v'
alias gra='git remote add'

# Checkouting
alias go='git checkout'
alias goo='git checkout --ours'
alias got='git checkout --theirs'

# Stashing
alias gt="git stash"
alias gtl="git stash list"
alias gtp="git stash pop"
alias gts="git stash show"
# Since stashes are pretty irrevokably lost if dropped, aliases for dropping
# was skipped

# Submodules
alias gsa='git submodule add'
alias gsi='git submodule init'
alias gsu='git submodule update'
alias gus='git pull && gsu'

# Diffing
alias gd='git diff'
alias gdh='git diff HEAD'
alias gdt='git difftool'

# Merging
alias gm="git merge"
alias gmt="git mergetool"
alias gls="git ls-files"
alias glsu="git ls-files --unmerged"
alias glsm="git ls-files --modified"
alias glss="git ls-files --stage"
alias glsd="git ls-files --deleted"

# Logging
alias gl='git log --abbrev-commit --pretty=oneline --decorate'
alias gll='git log --abbrev-commit --decorate --stat'
alias glg="gl --graph"
alias gllg="gll --graph"
alias glc="git shortlog --summary --numbered"
alias glr="git reflog"

alias gau='git update-index --assume-unchanged'


# Setup remote for a branch (mnemonic: git branch remote)
function gbr() {
    if [[ -n "$1" ]]; then
        branch=$1
        remote=${2:=origin}  # Optionally; what remote?

        git config branch.$branch.remote $remote
        git config branch.$branch.merge refs/heads/$1

        echo "Branch $branch now tracking $remote"
    else
        zerror "Tell me a branch, fool."
    fi
}

# Setup upstream for a branch (mnemonic: git branch upstream)
function gbu() {
    if [[ -n "$1" ]] && [[ -n "$2" ]]; then
        branch=$1
        remote=$2
        remote_branch=${3:=${1}}  # Optionally; which remote branch?

        git branch $branch --set-upstream $remote/$remote_branch
    else
        zerror "Tell me a branch and a remote, fool."
    fi
}

# Initialize a project
function ginit() {
    if [[ -z "$1" ]] ; then
        echo "Specify project name"
        return 1
    fi

    git init $1
    cd $1
    touch README
    git add README
    git commit -m "Initial commit"
}

# Record yourself for a project
function gcu() {
    if [[ -n "$1" ]] && [[ -n "$2" ]] ; then
        name="$1"
        email="$2"
    elif [[ -n "$FULLNAME" ]] && [[ -n "$EMAIL" ]] ; then
        name="$FULLNAME"
        email="$EMAIL"
    else
        zerror "gcu() needs two arguments or default data set in userfile."
        return 1
    fi

    git config user.name $name
    git config user.email $email
}

# Relatively go up to a repository root
function gr() {
    cur=$PWD
    found=false
    is_in=false

    if [[ -d "$cur/.git" ]]; then
        is_in=true
        cur=${cur%/*}
    fi

    until [[ -z "$cur" ]]; do
        if [[ -d "$cur/.git" ]]; then
            found=true
            break
        fi
        cur=${cur%/*}
    done

    if $found; then
        if $is_in; then
            echo "In submodule: going to superproject"
        fi

        echo $cur
        cd $cur
    elif [[ -d "$PWD/.git" ]]; then
        echo "Already at project root"
    else
        zerror "Currently not in a git repository"
    fi
}

zle -N _inline-gss
bindkey "^[s" _inline-gss

function _inline-gss () {
    # zle -M "$(git -c color.ui=always status --short)"
    zle -M "$(git status --short)"
}

# Love borrowed from mikachu!
zle -N _gitref
zle -N _quote_word
zle -N _unquote_word
bindkey "^[g"    _gitref

autoload -U modify-current-argument
autoload -U split-shell-arguments

function _quote_word()
{
  local q=qqqq
  modify-current-argument '${('$q[1,${NUMERIC:-1}]')ARG}'
}

function _unquote_word()
{
  modify-current-argument '${(Q)ARG}'
}

function _quote_unquote_word()
{
  local q=qqqq
  modify-current-argument '${('$q[1,${NUMERIC:-1}]')${(Q)ARG}}'
}

function _split_shell_arguments_under() {
    local -a reply
    split-shell-arguments
    #have to duplicate some of modify-current-argument to get the word
    #_under_ the cursor, not after.
    setopt localoptions noksharrays multibyte
    if (( REPLY > 1 )); then
        if (( REPLY & 1 )); then
            (( REPLY-- ))
        fi
    fi
    REPLY=${reply[$REPLY]}
}

function _gitref() {
    local REPLY
    local msg="Select one of s, S, t, a, c, v, q, Q, r${${1+.}:-, h (help).}"
    zle -R $msg
    read -k
    case $REPLY in
        (s)
            modify-current-argument '$(git rev-parse --short='${NUMERIC:-4}' ${(Q)ARG} 2> /dev/null)'
            ;;
        (S)
            modify-current-argument '$(git rev-parse ${(Q)ARG} 2> /dev/null)'
            ;;
        (t)
            modify-current-argument '$(git describe --tags ${(Q)ARG} 2> /dev/null)'
            ;;
        (a)
            modify-current-argument '$(git describe --all ${(Q)ARG} 2> /dev/null)'
            ;;
        (c)
            modify-current-argument '${(q)$(git describe --contains ${(Q)ARG} 2> /dev/null)}'
            ;;
        (v)
            if [[ -d $(git rev-parse --show-cdup).git/svn ]]; then
                modify-current-argument '$(git rev-parse --short='${NUMERIC:-4}' $(git svn find-rev r$ARG 2> /dev/null) 2> /dev/null || git svn find-rev $ARG 2> /dev/null)'
            else
                zle -R "Not a git-svn repo."
            fi
            ;;
        (q)
            _quote_word
            ;;
        (Q)
            _unquote_word
            ;;
        (w)
            modify-current-argument '$(git rev-list $ARG 2> /dev/null | wc -l)'
            ;;
        (r)
            local -a match mbegin mend
            modify-current-argument '${ARG//(#b)((#B)(*[^.])#)(#b)(.(#c2,3))((#B)([^.]*)#)/$match[3]$match[2]$match[1]}'
            ;;
        (h)
            [[ $1 = help ]] && return
            _split_shell_arguments_under
            word=$REPLY
            zle -M \
"
s: convert word to sha1, takes numeric argument (default: 4) ($(git rev-parse --short=${NUMERIC:-4} $word 2> /dev/null))
S: convert word to full length sha1 ($(git rev-parse $word 2> /dev/null))
t: convert word to described tag ($(git describe --tags $word 2> /dev/null))
a: convert word to described ref ($(git describe --all $word 2> /dev/null))
c: convert word to contained tag (${(q)$(git describe --contains $word 2> /dev/null)})
v: convert word with git svn find-rev
q: quote word
Q: unquote word
r: change order of range from a..b or a...b to b..a or b...a respectively
h: this help message"
            _gitref help
            ;;
        (*)
            [[ -n $REPLY ]] && repeat ${NUMERIC:-1}; do zle -U - $REPLY; done
            ;;
    esac
    zle -R -c
}
