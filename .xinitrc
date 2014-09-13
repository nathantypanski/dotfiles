XINITDEFAULT="/etc/X11/Xsession"
WALLPAPER="/home/nathan/pics/Wallpapers/TNY264GN-HD.jpg"

pidof xmodmap &> /dev/null
if [ "$?" -ne "0" ]; then
    /usr/bin/xmodmap /home/nathan/.xmodmap
fi

/usr/bin/xrdb -merge /home/nathan/.Xdefaults
/usr/bin/setxkbmap -layout us -geometry thinkpad -option ctrl:nocaps -option compose:menu &
/usr/bin/xsetroot -cursor_name left_ptr
/usr/bin/xset +fp /usr/share/fonts/local
/usr/bin/xset fp rehash
/usr/bin/xset r rate 200 50
/usr/bin/xinput --set-prop 13 'Device Accel Profile' 6
xmonad="/home/nathan/.xmonad/dist/build/my-xmonad/my-xmonad"
#xmonad="/home/nathan/.xmonad/.cabal-sandbox/bin/xmonad"
#GHC_PACKAGE_PATH="/home/nathan/.xmonad/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d":"/home/nathan/.ghc/x86_64-linux-7.8.3/package.conf.d"\
#    exec $xmonad
exec $xmonad
