#!/bin/bash
#
# pop-up calendar for dzen
#
# based on (c) 2007, by Robert Manea
# http://dzen.geekmode.org/dwiki/doku.php?id=dzen:calendar
# modified by urukrama
#

TODAY=$(expr `date +'%d'` + 0)
MONTH=`date +'%m'`
YEAR=`date +'%Y'`

(
echo '^bg(#000000)^fg(#F2EDD7)'`date +'%A %d %B %Y %n'`; echo
\
# current month, hilight header and today
cal \
    | sed -re "s/^(.*[A-Za-z][A-Za-z]*.*)$/^fg(#B23308)^bg(#000000)\1/;s/(^|[ ])($TODAY)($|[ ])/\1^bg(#B23308)^fg(#000000)\2^fg(#F2EDD7)^bg(#000000)\3/"

# next month, hilight header
#[ $MONTH -eq 12 ] && YEAR=`expr $YEAR + 1`
#cal `expr \( $MONTH + 1 \) % 12` $YEAR \
#    | sed -e 's/^\(.*[A-Za-z][A-Za-z]*.*\)$/^fg(#B23308)^bg(#000000)\1/'

sleep 8
) \
    | dzen2 -fg '#F2EDD7' -bg '#000000' -fn '-artwiz-nu-*-*-*-*-*-*-*-*-*-*-*-*' -x 864 -y 18 -w 160 -l 9 -sa c -e 'button3=exit;onstart=uncollapse;'-

#-artwiz-nu-medium-r-normal--11-110-75-75-p-90-iso8859-1
