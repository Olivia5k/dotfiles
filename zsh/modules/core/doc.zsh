function _zdoc()
{
	if [[ $2 =~ "(s(hort)?|l(ong)?)" ]] ; then
		echo $($1 "--zdoc" $2)
	fi
}

function zfunc()##
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Lists all local zsh functions and information about them."
		elif [[ "$2" =~ "l(ong)?" ]] ; then
			local str="Multiline string\n"
			str+="Second row"
			echo $str
		fi
		return
	fi

	local mutli=false
	private=false
	if [[ -n "$1" ]] ; then
		if [[ "$1" = "-a" ]] ; then
			private=true
			shift 1
		fi

		if [[ -n "$1" ]] ; then
			# Check if the given argument is a module
			if [[ $1 =~ ".zsh$" ]] && [[ $ZMODULES =~ $1 ]] || \
				[[ $ZMODULES =~ "\b$1.zsh\b" ]] ; then

				local module=$1
			else
				line=$(grep "function $1()" $ZFUNCDUMP)
				if [[ -n "$line" ]] ; then
					_zfuncformat $line true true
				else
					_zerror "%B$1%b is not a function local to this zsh install."
				fi
				return
			fi
		fi
	fi

	local IFS=$'\n' # That is weird.
	multi=true
	for line in $(cat $ZFUNCDUMP) ; do
		_zfuncformat $line false $private $module
	done
	unset line
	unset lastfile
}

function _zfuncformat()#
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Internal helper function to print information from zfunc()."
		fi
		return
	fi

	local line=$1
	local full=$2
	local private=$3
	local fdoc=false
	local ldoc=false

	file=$(echo $line | cut -f1 -d:)
	file=${file##*/}
	num=$(echo $line | cut -f2 -d:)
	func=$(echo $line | cut -f3 -d:)
	func=$func[10,-1]

	if $multi ; then
		# If the function is private and -a is not given, it is not to be printed.
		if ! $private && [[ $func[1] = "_" ]] ; then
			continue
		fi

		# If the module is not loaded, mutliit
		if ! [[ $ZMODULES =~ $file ]] ; then
			continue
		fi

		if [[ -n "$module" ]] && [[ $file != "${module}.zsh" ]] ; then
			continue
		fi

		if [[ -n "$lastfile" ]] && [[ "$file" != "$lastfile" ]] ; then
			echo
			echo "$file:"
		fi
	fi

	if [[ "$func[-1]" = "#" ]] ; then
		fdoc=true
		if [[ "$func[-2]" = "#" ]] ; then
			ldoc=true
			x="-5"
		else
			x="-4"
		fi
	else
		x="-3"
	fi

	func=$func[1,$x]
	s="%F{blue}%B${func}()%b%f"

	if $fdoc ; then
		s+=": $($func --zdoc s)"
	else
		s+=": %F{white}Undocumented.%f"
	fi

	if $full ; then
		s+="\nFound inside %F{blue}%B${file}%b%f:%F{red}%B${num}%b%f"
		s+="\n\n"
		if $ldoc; then
			s+="$($func --zdoc l)"
		else
			s+="%F{white}No further information available.%f"
		fi
	#else
		#s+=" %F{green}($file:$num)%f"
	fi

	print -P $s
	lastfile=$file
}

function _zdocex()
{
	if [[ "$1" = "--zdoc" ]] ; then
		if [[ "$2" =~ "s(hort)?" ]] ; then
			echo "Short desc"
		elif [[ "$2" =~ "l(ong)?" ]] ; then
			echo "Long desc"
		fi
		return
	fi
}

# Dump data on all local functions for use with the documentation functions
export ZFUNCDUMP="/tmp/zshfuncdump-$USER"
grep -ERin "^function" $ZSHCONFDIR > $ZFUNCDUMP
