function cl()
{
	cd $1 && ls
}
function cll()
{
cd $1 && ls -l
}
function clal()
{
	cd $1 && ls -al
}

alias '..'='cd ..'
alias -g '...'='../..'
alias -g '....'='../../..'
alias -g '.....'='../../../..'
