#!/bin/sh

# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# I like to start X from bad directories when I'm working, so this fixes my mistakes!
cd ~/

# background
feh --bg-scale pics/Wallpapers/nin_plug.jpg

# US keyboard layout and thinkpad
setxkbmap -layout us -geometry thinkpad &

# load my custom keybindings
xbindkeys &

## solid background color
# exec --no-startup-id hsetroot -solid '#002b36'

# set a mouse pointer
xsetroot -cursor_name left_ptr &

# load x resources db
xrdb -merge ~/.Xresources &

# automounting
udiskie &

# hides the mouse pointer after a grace period
unclutter &

### Now I'm on optimus! Who cares about nvidia settings?
# nvidia-settings -l

synclient TouchpadOff=1 &

# Set the font path ...
xset +fp /usr/share/fonts/local &

# Rehash for good measure
xset fp rehash &

# mouse acceleration as "the exponent of a more natural and continuous formula"
xset m 3/2 0 &

# compose
setxkbmap -option compose:menu &

# Finally!

## i3 with logging
exec i3 -V >> ~/.i3/i3log 2>&1
#exec xmonad
#exec ~/ob/openbox-3.5.0/openbox/openbox
