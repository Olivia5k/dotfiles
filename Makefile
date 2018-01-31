PACKAGES := $(shell find -maxdepth 1 -type d -not -name .git -not -name . | sed -e 's/^\.\///' | sort)
STOW := ./stow

.DEFAULT_GOAL: all
.PHONY: $(PACKAGES)
all: $(PACKAGES)

keymaps:
	sudo $(STOW) -v --target=/usr/share/X11/xkb/symbols/ keymaps

util:
	$(STOW) -v --target=$(HOME)/.local util

emacs git tmux zsh x11 polybar:
	$(STOW) -v --target=$(HOME) $@
