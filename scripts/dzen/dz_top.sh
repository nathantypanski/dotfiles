#!/bin/sh

FG='#839496'
BG='#00141a'
FONT='Terminus-8'
CONKYCONFIG='/home/nathan/.conkytoprc'

(conky -c '/home/nathan/.conkytoprc'; cat '~/.zshrc')| dzen2 -dock -ta r -h '16' -w '1600' -bg $BG -fg $FG -fn $FONT -e 'button2=' &
