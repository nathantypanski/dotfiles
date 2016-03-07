#!/bin/bash

# One-liner version:
# import -window root png:- | convert - -blur 0x20 png:- | i3lock -n -f -i /dev/stdin
scrot /tmp/lock.png
i3lock -f -i /tmp/lock.png
convert /tmp/lock.png -blur 0x8 png:- > /tmp/lockblur.png 
killall i3lock
i3lock -n -f -i /tmp/lockblur.png
