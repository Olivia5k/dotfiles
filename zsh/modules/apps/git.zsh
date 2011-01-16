export HASCVS=true

# Committing
alias ga='git add'
alias gs='git status'
alias gb='git branch'
alias gc='git commit -a'
alias gp='git push'
alias gu='git pull'

# Checkouting
alias go='git checkout'
alias goo='git checkout --ours'
alias got='git checkout --theirs'

# Submodules
alias gsu='git submodule update'
alias gus='git pull && gsu'

# Diffing
alias gd='git diff'
alias gdt='git difftool'
alias gm="git merge"

# Logging
alias gl='git log --pretty=oneline --decorate'
alias gll='git log --decorate --stat'
alias glg="gl --graph"
alias gllg="gll --graph"
alias gsl="git shortlog"
alias gss="git shortlog --summary --numbered"
alias grl="git reflog"

alias gau='git update-index --assume-unchanged'

# Setup remote for a branch
function gbr()
{
	if [ -n "$1" ] ; then
		git config branch.$1.remote origin
		git config branch.$1.merge refs/heads/$1
	else
		echo "Tell me a branch, fool."
	fi
}

# Initialize a project
function gi()
{
	if [ -z "$1" ] ; then ; echo "Specify project name" && return 1 ; fi

	git init $1
	cd $1
	touch README
	git add README
	git commit -m "Initial commit"
}

# Record yourself for a project
function gcu()
{
	if [ -n "$1" ] && [ -n "$2" ] ; then
		name="$1"
		email="$2"
	elif [ -n "$FULLNAME" ] && [ -n "$EMAIL" ] ; then
		zdebug "Not enough arguments; defaulting to default data"
		name="$FULLNAME"
		email="$EMAIL"
	else
		zerror "gcu() needs two arguments or default data set in userfile."
		return 1
	fi

	git config user.name $name
	git config user.email $email
}
