#!/bin/sh

# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

cd ~/

# load x resources db
xrdb -merge ~/.Xresources &

# terminus-font is installed here, but it's not in the font path by default.
xset +fp /usr/share/fonts/local &
xset fp rehash &

# background
# feh --bg-scale pics/Wallpapers/nin_plug.jpg

# US keyboard layout and thinkpad
setxkbmap -layout us -geometry thinkpad &

# compose
setxkbmap -option compose:menu &

# load my custom keybindings
# xbindkeys &

# set a mouse pointer
xsetroot -cursor_name left_ptr &

# automounting
udiskie &

# hides the mouse pointer after a grace period
unclutter &

### Now I'm on optimus! Who cares about nvidia settings?
# nvidia-settings -l

# disable my touchpad
synclient TouchpadOff=1 &

# mouse acceleration as "the exponent of a more natural and continuous formula"
xset m 3/2 0 &

# compositing
compton &

#/usr/lib/systemd --user

# Finally!

## i3 with logging
#exec i3 -V >> ~/.i3/i3log 2>&1
exec xmonad
#exec openbox
