export MPD_HOST=localhost
export MPD_PORT=6600

alias np='mpc --format "%position%) %artist% #[%album%#] - %title%"'
alias mps='np toggle' # Stop
alias mpl='np playlist'
alias mpa='mpc add'
alias mpx='mpc clear'
alias mpll='mpc load' # Playlist load
alias mpls='mpx && mpll' # Playlist switch
alias mpp='np play'
alias mpr='np random'

if has mpcext ; then
	alias mpg='mpcext -s'
	alias mpag='mpcext -S' # grep all
	alias mpgs='mpcext -sw' # grep switch
	alias mpq='mpcext -q' # Queue
fi
