export HASCVS=true


#zstyle ':vcs_info:*' enable git
#zstyle ':vcs_info:git*:*' get-revision true
#zstyle ':vcs_info:git*:*' check-for-changes true

## hash changes branch misc
#zstyle ':vcs_info:git*' formats "%f%7.7i %c%u %b%m"
#zstyle ':vcs_info:git*' actionformats "(%s|%a) %7.7i %c%u %b%m"

zstyle ':vcs_info:git*+set-message:*' hooks git-stash

## Show remote ref name and number of commits ahead-of or behind
#function +vi-git-st() {
    #local ahead behind remote
    #local -a gitstatus

    ## Are we on a remote-tracking branch?
    #remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        #--symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    #if [[ -n ${remote} ]] ; then
        ## for git prior to 1.7
        ## ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
        #ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
        #(( $ahead )) && gitstatus+=( "%F{${c[15]}}+${ahead}${c2}" )

        ## for git prior to 1.7
        ## behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
        #behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
        #(( $behind )) && gitstatus+=( "%F{${c[16]}}-${behind}${c2}" )

        #hook_com[branch]="${hook_com[branch]} [${(j:/:)gitstatus}]"
    #fi
#}

# Show count of stashed changes
function +vi-git-stash() {
    local -a stashes

    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l)
        hook_com[misc]+=" (${stashes} st)"
    fi
}


# Committing
alias ga='git add'
alias gs='git status'
alias gc='git commit'
alias gca='git commit -a'
alias gp='git push'
alias gu='git pull'

# Branching
alias gb='git branch -v'

# Checkouting
alias go='git checkout'
alias goo='git checkout --ours'
alias got='git checkout --theirs'

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
