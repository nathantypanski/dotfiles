#!/bin/sh

FG='#839496'
BG='#00141a'
FONT='Terminus-8'
CONKYCONFIG='/home/nathan/.conkybottomrc'

conky -c $CONKYCONFIG | dzen2 -ta r -h '16' -x '300' -y '884' -w '800' -bg $BG -fg $FG -fn $FONT -e 'button2=' &
