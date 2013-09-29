#!/bin/bash
# Script for setting up DisplayLink monitor.
#
# Fill in MAINOUTPUT, MAINPROVIDER, WIDTH, HEIGHT, and REFRESH with your own
# values before use.

MAINOUTPUT="LVDS1" # Obtained from xrandr --current
MAINPROVIDER="0"   # Obtained from xrandr --listproviders
WIDTH="1366"       # Width of DisplayLink monitor in pixels
HEIGHT="768"       # Height of DisplayLink monitor in pixels
REFRESH="59.9"     # Refresh rate of DisplayLink monitor

modeline=$(gtf $WIDTH $HEIGHT $REFRESH | grep -E "Modeline" | cut -d ' ' -f 4- |
           sed -e 's/"//g')
dlmodename=$(echo $modeline | cut -d ' ' -f 1)
dlprovider=$(xrandr --listproviders | grep modesetting | cut -d ' ' -f 2 | 
              sed -e 's/://g')
mainstatus=$(xrandr --current | grep $MAINOUTPUT | cut -d ' ' -f 3)
SEARCHLINE='\([0-9]\+\)x\([0-9]\+\)+\([0-9]\+\)+\([0-9]\+\)'
mainwidth=$(echo $mainstatus | sed 's/'$SEARCHLINE'/\1/g')
mainheight=$(echo $mainstatus | sed 's/'$SEARCHLINE'/\2/g')
mainx=$(echo $mainstatus | sed 's/'$SEARCHLINE'/\3/g')
mainy=$(echo $mainstatus | sed 's/'$SEARCHLINE'/\4/g')

tput setaf 6; tput bold;
printf ">> DisplayLink Mode: "
tput sgr0;
echo $modeline

tput setaf 6; tput bold;
printf ">> DisplayLink Provider: "
tput sgr0;
echo $dlprovider

tput setaf 6; tput bold;
printf ">> DisplayLink Mode name: "
tput sgr0;
echo $dlmodename

if ! xrandr | grep DVI; then
    xrandr --setprovideroutputsource $dlprovider $MAINPROVIDER
fi

if ! xrandr | grep $dlmodename; then
    tput setaf 6; tput bold;
    printf ">> Desired DisplayLink mode "
    tput sgr0;
    printf $dlmodename
    tput setaf 6; tput bold;
    printf " does not exist! Creating new mode ...\n"
    xrandr --newmode $modeline
fi

tput setaf 6; tput bold;
printf ">> DisplayLink output: "
tput sgr0;
dloutput=$(xrandr | grep DVI | cut -d ' ' -f 1)
echo $dloutput

xrandr --addmode $dloutput $dlmodename
xrandr --output $MAINOUTPUT --mode "$mainwidth"x"$mainheight" --pos "$mainx"x"$mainy" --rotate normal \
       --output $dloutput --mode $dlmodename --pos "$mainwidth"x"$mainy" --rotate normal \
       --output VGA1 --off
