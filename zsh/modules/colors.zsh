# Prints a table of the 16 default colors. Usually comes as a standalone
# script, but hey, why not integrate it? :>
function c()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Integrated version of the most spread colorscheme tester."
		fi
		return
	fi
	T='HaX' # The test text
	echo -e "                 40m     41m     42m     43m     44m     45m     46m     47m";

	for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
		'1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
		'  36m' '1;36m' '  37m' '1;37m';
	do
		FG=${FGs// /}
		echo -en " $FGs \033[$FG  $T  "
		for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
			# srsly, where does EINS come from? :(
			do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
		done
		echo;
	done
}

# Prints a table of the 256 extended colors. Incredibly useful when making
# terminal editor colorschemes. The default prints quite wide, so when in a
# small terminal, it prints a simplified version. Given any arguments, the large
# one is printed regardless.
function C()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Prints a small table of the 256 extended colors."
		fi
		return
	fi
	for line in {0..15}; do
		for col in {0..15}; do
			local code=$(( $col * 16 + ${line} ))
			printf $'\e[38;05;%dm %03d' ${code} ${code}
		done
	print ; done
}

function CC()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Prints a large table of the 256 extended colors."
		fi
		return
	fi
	for i in {16..255}; do
		echo -en "\e[38;5;${i}mColor $i\t"
		[ $((++j%6)) -eq 0 ] && echo
	done
}
