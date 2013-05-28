#!/bin/sh

FG='#839496'
BG='#00141a'
FONT='Terminus-8'
CONKYCONFIG='scripts/dzen/conkybottomrc'

#conky -c $CONKYCONFIG | dzen2 -dock -ta l -h '16' -y '884' -x 200 -bg $BG -fg $FG -fn $FONT -e 'button2=' &
conky -c $CONKYCONFIG | dzen2 -dock -ta l -h '16' -y '884' -bg $BG -fg $FG -fn $FONT -e 'button2=' &
(conky -c 'scripts/dzen/conky_cal'; cal) | dzen2 -ta r -y 886 -w 150 -x 1450 -l 7 -bg $BG -fg $FG -fn $FONT -e 'button2=;entertitle=exec cal,uncollapse,unhide;leaveslave=collapse;leavetitle=collapse' &
