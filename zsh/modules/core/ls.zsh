if _has ls++; then
    ls="ls++ --group-directories-first"
    alias l="/bin/ls $LSOPTS"
else
    ls="ls $LSOPTS"
fi

alias ls="$ls"
alias lc="$ls --color=never"
alias ll="$ls -l"
alias la="$ls -A"
alias lal="$ls -Al"

alias lsd="$ls -d *(-/N)"          # list visible directories
alias lsf="$ls *(-.N)"             # list visible files

alias lad="$ls -d *(-/DN)"         # list all directories
alias laf="$ls -A *(-.DN)"         # list all files
alias llad="$ls -lhd *(-/DN)"      # list details of all directories
alias llaf="$ls -lhA *(-.DN)"      # list details of all files
alias lld="$ls -lhd *(-/N)"        # list details of visible directories
alias llf="$ls -lh *(-.N)"         # list details of visible files
alias lh="$ls -d .*"               # list hidden files/directories
alias lhd="$ls -d .*(-/N)"         # list hidden directories
alias lhf="$ls .*(-.N)"            # list hidden files
alias llh="$ls -lhd .*"            # list details of hidden files/directories
alias llhd="$ls -lhd .*(-/N)"      # list details of hidden directories
alias llhf="$ls -lh .*(-.N)"       # list details of hidden files

alias le="$ls -d *(-/DN^F)"        # list all empty directories
alias ler="$ls -d **/*(-/DN^F)"    # list all empty directories recursively
alias lle="$ls -ld *(-/DN^F)"      # list details of all empty directories
alias ller="$ls -lhd **/*(-/DN^F)" # list details of all empty directories recursively

function lsx() {
    ls *.$1
}
