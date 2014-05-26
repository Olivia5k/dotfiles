# tmux

![wow screenshot](http://i.imgur.com/dQf6brW.png)

Screenshot of the `tmux` tmux session demonstrating what my tmux configuration
looks like. The top split is in copy mode and the bottom right is in
choose-session mode.

This tmux configuration is based on the notion that tmux is really really
useful, but it's default bindings are beyond silly.

All bindings below assume that you've already entered the prefix key, `^A`.


### The best feature evar

* `^C`: Enter copy mode

When in copy mode, you can browse the scrollback of your terminal (which is btw
set really high). In it, you can use vim bindings (without prefixing them) to
search and navigate. This is useful either for reading what the hell just
happened, or for copying something. To copy something, press `Space`. When in
that mode, you can toggle block and normal mode with `v`. To select something,
press `Enter`.

* `p`: Paste current clipboard
* `P`: Choose what to paste


### Windows

* `c`: Create a new window
* `M-hl`: Move windows forward or backward
* `M-w`: Force kill a window

* `^A`: Go to last window
* `Space`: Next window (repeats)
* `Backspace`: Previous window (repeats)
* `w`: Choose a window

* `r`: Rename current window

* `^S`: Prompt for a hostname and create a dedicated ssh window to that host
* `^M`: Prompt for a manpage and create a dedicated window for that manpage
* `^J`: Prompt for a binary and create a dedicated window for running that
* `^H`: Dedicated [htop][htop] window

### Panes

* `s`: Split vertically (will retain $PWD for the split)
* `z`: Split horizontally (see above)
* `M-p`: Force kill a pane

* `hjkl`: Browse between panes
* `HJKL`: Resize pane (repeats)
* `M-jk`: Switch panes up or down (repeats)

* `b`: Break pane to it's own window
* `x`: Make pane fullscreen. Run again to restore.

* `S`: Prompt for a hostname and create a ssh vertical pane to that host
* `Z`: Prompt for a hostname and create a ssh horizontal pane to that host


### Sessions

* `o`: Choose active session
* `N`: Prompt for session name and select it. Create it if it does not exist.
  This will not work in versions less than 1.7, since it needs the -A flag for
  new-session.


### Toggling and misc

* `E/e` Set statusbar on/off
* `I/i` Set monitor-activity on/off
* `Y/y` Set synchronize-panes on/off

* `^D`: Detach current session
* `^R`: Reload configuration file

The statusbar was inspired by [powerline][powerline].


[htop]: http://htop.sourceforge.net/
[powerline]: https://github.com/Lokaltog/powerline
