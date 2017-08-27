PACKAGES := $(shell find -maxdepth 1 -type d -not -name .git -not -name . | sed -e 's/^\.\///' | sort)

.DEFAULT_GOAL: all

.PHONY: $(PACKAGES)

all: $(PACKAGES)

keymaps:
	sudo stow -v --target=/usr/share/X11/xkb/symbols/ keymaps

util:
	stow -v --target=$(HOME)/.local util

emacs git stumpwm tmux zsh x11:
	stow -v --target=$(HOME) $@
