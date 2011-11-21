export HASCVS=true


zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*:*' get-revision true
zstyle ':vcs_info:git*:*' check-for-changes true

f="%B%F{${c[14]}}%r%F{${c[1]}}(%F{${c[15]}}%u%c%b%m%F{${c[1]}}):%F{${c[4]}}/%S"
## hash changes branch misc
zstyle ':vcs_info:git*' formats "%f%7.7i %c%u %b%m"
zstyle ':vcs_info:git*' formats $f
zstyle ':vcs_info:git*' actionformats "(%s|%a) %7.7i %c%u %b%m"

zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash

# Show remote ref name and number of commits ahead-of or behind
function +vi-git-st()
{
    local str branch ahead behind remote

    # Store the current branch name
    branch=${hook_com[branch]}

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${b}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} ]] ; then
        # Dark colon to mark tracking branch
        str="${branch}%F{${c[1]}}:"

        ahead=$(git rev-list ${branch}@{upstream}..HEAD 2>/dev/null | wc -l)
        behind=$(git rev-list HEAD..${branch}@{upstream} 2>/dev/null | wc -l)

        (( $ahead )) && str+="%F{${c[15]}}+${ahead}%f"
        (( $ahead )) && (( $behind )) && str+="%F{${c[1]}}/"
        (( $behind )) && str+="%F{${c[16]}}-${behind}%f"
        (( $ahead + $behind )) && str+="%F{${c[1]}}:"
    else
        # Just add a red colon to mark non-tracking branch
        str="${branch}%F{${c[16]}}:"
    fi

    hook_com[branch]=$str
}

 #Show count of stashed changes
function +vi-git-stash()
{
    local -a stashes

    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l)
        hook_com[misc]+="%F{${c[4]}}${stashes}st%F{${c[1]}}"
    fi
}


# Committing / General
alias ga='git add'
alias gs='git status'
alias gc='git commit'
alias gca='git commit -a'
alias gp='git push'
alias gu='git pull'

# Branching
alias gb='git branch -v'
alias gbv='git branch -vv'

# Checkouting
alias go='git checkout'
alias goo='git checkout --ours'
alias got='git checkout --theirs'

# Stashing
alias gt="git stash"
alias gtl="git stash list"
alias gtp="git stash pop"
alias gts="git stash show"
# Since these are pretty irrevokably lost if dropped, aliases for that was
# skipped

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

# Setup remote for a branch
function gbr()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Setup remote for a branch"
        fi
        return
    fi

    if [[ -n "$1" ]] ; then
        git config branch.$1.remote origin
        git config branch.$1.merge refs/heads/$1
    else
        echo "Tell me a branch, fool."
    fi
}

# Initialize a project
function ginit()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Initialize a new git project"
        fi
        return
    fi

    if [[ -z "$1" ]] ; then ; echo "Specify project name" && return 1 ; fi

    git init $1
    cd $1
    touch README
    git add README
    git commit -m "Initial commit"
}

# Record yourself for a project
function gcu()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Record yourself for a project"
        fi
        return
    fi

    if [[ -n "$1" ]] && [[ -n "$2" ]] ; then
        name="$1"
        email="$2"
    elif [[ -n "$FULLNAME" ]] && [[ -n "$EMAIL" ]] ; then
        _zdebug "Not enough arguments; defaulting to default data"
        name="$FULLNAME"
        email="$EMAIL"
    else
        _zerror "gcu() needs two arguments or default data set in userfile."
        return 1
    fi

    git config user.name $name
    git config user.email $email
}

# Relatively go up to a repository root
function gr()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Relatively go up to a repository root"
        fi
        return
    fi

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

    if $is_in && $found; then
        echo "In submodule: going to superproject"
        echo $cur
        cd $cur
    elif $found; then
        echo $cur
        cd $cur
    elif [[ -d "$PWD/.git" ]] ; then
        echo "Already at project root"
    else
        _zerror "Currently not in a git repository"
    fi
}

# Fetch and merge a branch
function gmr()#
{
    if [[ "$1" = "--zdoc" ]] ; then
        if [[ "$2" =~ "s(hort)?" ]] ; then
            echo "Fetch and merge a branch."
        fi
        return
    fi

    if [[ -n "$2" ]]; then
        remote="$1"
        branch="$2"
    else
        remote="origin"
        branch="$1"
    fi

    git fetch $remote $branch
    git merge $remote/$branch
}
