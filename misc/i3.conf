# font -misc-fixed-medium-r-normal--13-120-75-75-C-70-iso10646-1
# font -xos4-terminus-bold-*-*-*-14-*-*-*-*-*-*-*
font -*-montecarlo-*-*-*-*-*-*-*-*-*-*-*-*

# Use Mouse+Mod4 to drag floating windows to their wanted position
floating_modifier Mod4

# Toggle tiling/floating of the current window (Mod4+Shift+Space)
bindcode Mod4+Shift+65 floating toggle

# Go into the tiling layer / floating layer, depending on whether
# the current window is tiling / floating (Mod4+Mod1+f)
bindsym Mod4+Mod1+f focus mode_toggle

# Focus (Mod4+hjkl)
bindsym Mod4+h focus left
bindsym Mod4+j focus down
bindsym Mod4+k focus up
bindsym Mod4+l focus right
bindsym Mod4+u focus parent
#bindsym Mod4+b focus child

# Splitting
bindsym Mod4+Control+z split vertical
bindsym Mod4+Control+s split horizontal

# Snap (Mod4+Control+hjkl)
bindsym Mod4+Control+h layout stacking
bindsym Mod4+Control+j layout default
bindsym Mod4+Control+k layout tabbed
bindsym Mod4+Control+l fullscreen

# Move (Mod4+Shift+hjkl)
bindsym Mod4+Shift+h move left
bindsym Mod4+Shift+j move down
bindsym Mod4+Shift+k move up
bindsym Mod4+Shift+l move right

# Move Container (Mod4+Control+Shift+hjkl)
bindsym Mod4+Control+Shift+h focus parent; move left
bindsym Mod4+Control+Shift+j focus parent; move down
bindsym Mod4+Control+Shift+k focus parent; move up
bindsym Mod4+Control+Shift+l focus parent; move right

# Relative workspace switch (Mod4+Mod1+hl)
#bindsym Mod4+Mod1+43 workspace prev
#bindsym Mod4+Mod1+46 workspace next

# Workspaces (Mod4+\d)
bindsym Mod4+1 workspace 1
bindsym Mod4+2 workspace 2
bindsym Mod4+3 workspace 3
bindsym Mod4+4 workspace 4
bindsym Mod4+5 workspace 5
bindsym Mod4+6 workspace 6
bindsym Mod4+7 workspace 7
bindsym Mod4+8 workspace 8
bindsym Mod4+9 workspace 9

# Move to Workspaces
bindsym Mod4+Shift+1 move workspace 1
bindsym Mod4+Shift+2 move workspace 2
bindsym Mod4+Shift+3 move workspace 3
bindsym Mod4+Shift+4 move workspace 4
bindsym Mod4+Shift+5 move workspace 5
bindsym Mod4+Shift+6 move workspace 6
bindsym Mod4+Shift+7 move workspace 7
bindsym Mod4+Shift+8 move workspace 8
bindsym Mod4+Shift+9 move workspace 9

# Assigning
assign [class="float"] ~

# Mod4+Enter starts a new terminal
bindcode Mod4+36 exec mor main
bindcode Mod4+Control+36 exec mor msc
bindcode Mod4+Mod1+36 exec urxvt

# Mod4+Mod1+k kills the current client
bindsym Mod4+Mod1+x kill

# dmenu hax.
# Mod4+Ctrl+Space for dmenu application launcher
bindcode Mod4+Control+65 exec dmenu_exec

# Mod4+Space launches default mux; alternate with Mod1
bindcode Mod4+65 exec dmenu_ssh
bindcode Mod4+Mod1+65 exec dmenu_mux

# Run-or-raisers
bindsym Mod4+a exec mor alt
bindsym Mod4+e exec mor dc
bindsym Mod4+c exec mor conf
bindsym Mod4+d exec mor dev
# bindsym Mod4+e exec mor main
bindsym Mod4+i exec mor irc
bindsym Mod4+m exec mor msc
bindsym Mod4+p exec mor python
bindsym Mod4+r exec mor rs
bindsym Mod4+t exec mor task
bindsym Mod4+w exec mor wrz

bindsym Mod4+q exec ror "chromium" "Chromium" "chromium"
bindsym Mod4+s exec ror "uzbl-browser" "Uzbl-core" "uzbl-browser $HOST.local"

# Mod4+g for dzentinel
bindsym Mod4+g exec dzentinel
bindsym Mod4+Control+g exec bash -c "pkill dzen"

# LOCKDOWN (Mod4+Backspace)
bindcode Mod4+22 exec xlock

# Volume mods
bindsym Mod4+z exec amixer set Master toggle
bindsym Mod4+Mod1+h exec amixer set PCM 10-
bindsym Mod4+Mod1+l exec amixer set PCM 10+

bindsym Mod4+Mod1+1 exec amixer set PCM 10%
bindsym Mod4+Mod1+2 exec amixer set PCM 20%
bindsym Mod4+Mod1+3 exec amixer set PCM 30%
bindsym Mod4+Mod1+4 exec amixer set PCM 40%
bindsym Mod4+Mod1+5 exec amixer set PCM 50%
bindsym Mod4+Mod1+6 exec amixer set PCM 60%
bindsym Mod4+Mod1+7 exec amixer set PCM 70%
bindsym Mod4+Mod1+8 exec amixer set PCM 80%
bindsym Mod4+Mod1+9 exec amixer set PCM 90%
bindsym Mod4+Mod1+0 exec amixer set Master toggle

# Simple MPD controls
bindsym Mod4+Mod1+a exec mpc -q prev
bindsym Mod4+Mod1+s exec mpc -q toggle
bindsym Mod4+Mod1+d exec mpc -q next

# Control screen output (Mod4+å)
bindcode Mod4+34 exec sswitch

# Hehe restart gunicorn (Mod4+.)
bindcode Mod4+60 exec ssh nl bin/gkill

# Lolz screenshot
bindsym Mod4+Mod1+p exec ss

# Rotate the screen!
#bindsym 121 exec srotate

# Mod4+Control+e exits i3
bind Mod4+Control+26 exit

# Mod4+Mod1+Control+r restarts i3 inplace
bindsym Mod4+Mod1+Control+r restart

# Mod4+Control+r reloads the configuration
bindsym Mod4+Control+r reload

# The IPC interface allows programs like an external workspace bar
# (i3-wsbar) or i3-msg (can be used to "remote-control" i3) to work.
ipc-socket ~/.local/share/i3/ipc.sock

# accually borders plx
new_window 1pixel

# Border togglers!
bindsym Mod4+y border 1pixel
bindsym Mod4+Control+y border default
bindsym Mod4+Mod1+Control+y border none

workspace_layout default

client.background #101010

exec_always bash -c "pkill dzen"
exec_always dzentinel
