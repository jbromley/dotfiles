# Xinitrc for window manager environment

# Set the wallpaper 
[ -x ${HOME}/.fehbg ] && ${HOME}/.fehbg || xsetroot -solid black &

# Read in X resource configuration
xrdb -merge "${HOME}/.Xresources"

dbus-daemon --print-address 4 --session
compton --daemon
xautolock -detectsleep -time 5 -locker "i3lock -e -f -i ${HOME}/.cache/lockscreen.png" &
udiskie --automount --notify --no-tray &

# Configure natural scrolling on pointing devices.
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled Default' 1
xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled' 1 
xinput set-prop 'Synaptics TM3053-003' 'Synaptics Scrolling Distance' -44 -44 

# Set up keyboard maps and switching.
setxkbmap -layout us,es -option 'grp:rctrl_rshift_toggle' -option 'caps:escape'

# Check monitor.
if xrandr | grep -qs -E '^DP-1-1 connected'; then
     xrandr --output eDP-1-1 --off --output DP-1-1 --primary --mode 2560x1440 --pos 0x0 --rotate normal --dpi 109 
fi

# Start dunst.
dunst &

exec ssh-agent /usr/bin/xmonad
