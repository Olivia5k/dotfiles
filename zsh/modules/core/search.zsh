function _clearbelowprompt() {
    zle -M ""
}

function _show_surroundings() {
    #zle -M ${"$(eval 'for a in $history[(I)<'$((HISTNO-10))-$((HISTNO+10))'>]; do if [[ $a -eq $HISTNO ]]; then printf '\''\n%s: %s\n\n'\'' $a $history[$a]; else; printf '\''%s: %s\n'\'' $a $history[$a]; fi; done')"}
    #zle -M ${"$(eval 'for a in $history[(I)<'$((HISTNO-10))-$((HISTNO+10))'>]; do printf '\''%s: %s\n'\'' $a $history[$a]; done')"}
    (( $LINES < 5 )) && { zle -M ""; return }
    local bound line
    typeset -a output
    typeset -A star
    bound=${NUMERIC:-$(( LINES < 10 ? 1 : LINES / 3 ))}
    star[$HISTNO]="*"
    for ((i = HISTNO - $bound; i < HISTNO + $bound && i < HISTCMD; i++)); do
        line="${${:-$star[$i]$i: $history[$i]}[1,COLUMNS-1]}"
        while (( ${(m)#line} > COLUMNS-1 )); do
        line[-1]=
        #for broken zsh#line=$line[1,-2]
        done
        output=( $line $output )
    done
    zle -M ${(pj:\n:)output}
}
zle -N zle-isearch-update _show_surroundings
zle -N zle-isearch-exit _clearbelowprompt
