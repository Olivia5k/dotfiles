function touch_init() {
    until [[ -z "$1" ]] && [[ -f "${1:-.}/__init__.py" ]]; do
        touch ${1:-.}/__init__.py
        shift &> /dev/null
    done
}
alias ti="touch_init"
