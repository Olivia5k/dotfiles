export HASCVS=true

# Committing
alias ga='git add'
alias gs='git status'
alias gb='git branch'
alias gc='git commit -a'
alias gp='git push'
alias gu='git pull'
alias gm="git merge"

# Checkouting
alias go='git checkout'
alias goo='git checkout --ours'
alias got='git checkout --theirs'

# Submodules
alias gsu='git submodule update'
alias gus='git pull && gsu'

# Diffing
alias gd='git diff'
alias gdh='git diff HEAD'
alias gdt='git difftool'

# Merging
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
function gi()#
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
    until [[ -z "$cur" ]]; do
        if [[ -d "$cur/.git" ]]; then
            found=true
            break
        fi
        cur=${cur%/*}
    done

    if $found; then
        echo $cur
        cd $cur
    else
        _zerror "Currently not in a git repository"
    fi
}
