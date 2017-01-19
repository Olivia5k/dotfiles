set -x PATH $PATH $HOME/.local/bin/
set -x PATH $PATH $HOME/git/dotfiles/util

set -x WORK_EMAIL thiderman@spotify.com
set -x WORK_REMOTE ghe
set -x PAYMENTS_SRC $HOME/spotify/payments/

set -x DOCKER_HOST localhost

set -g theme_color_scheme gruvbox

# Disable Ctrl-q/s flow control.
stty -ixon

# Fix ls so that we group directories
function ls --description "List contents of directory"
	set -l param --color=auto --group-directories-first
	command ls $param $argv
end
