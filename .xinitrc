XINITDEFAULT="/etc/X11/Xsession"
WALLPAPER="/home/nathan/pics/Wallpapers/TNY264GN-HD.jpg"

#if [[ -r "$XINITDEFAULT" &&  -x "$XINITDEFAULT" ]]; then
#    $XINITDEFAULT
#    echo "Loaded defaults."
#fi

pidof emacs &> /dev/null
if [ "$?" -ne "0" ]; then
    /usr/bin/emacs --daemon
fi

pidof urxvtd &> /dev/null
if [ "$?" -ne "0" ]; then
    /usr/bin/urxvtd -q -f
fi

pidof xbindkeys &> /dev/null
if [ "$?" -ne "0" ]; then
    /usr/bin/xbindkeys -n
fi

pidof xmodmap &> /dev/null
if [ "$?" -ne "0" ]; then
    /usr/bin/xmodmap /home/nathan/.xmodmap
fi

/usr/bin/xrdb -merge /home/nathan/.Xdefaults
/usr/bin/setxkbmap -layout us -geometry thinkpad -option ctrl:nocaps -option compose:menu &
/usr/bin/xsetroot -cursor_name left_ptr 
/usr/bin/xset +fp /usr/share/fonts/local
/usr/bin/xset fp rehash 
/usr/bin/xset r rate 200 30 
/usr/bin/xinput --set-prop 13 'Device Accel Profile' 6
#/usr/bin/feh --bg-fill $WALLPAPER &
#/usr/bin/hsetroot -cover $WALLPAPER 
#xmonad="/home/nathan/.xmonad/dist/build/my-xmonad/my-xmonad"
xmonad="/home/nathan/.xmonad/xmonad"
#exec dbus-launch --exit-with-session ck-launch-session $xmonad
exec xmonad
