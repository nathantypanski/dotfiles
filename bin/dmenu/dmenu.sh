#!/bin/sh

# Just runs dmenu, with my settings.

NORMAL_BACKGROUND='#1D1F21'
SELECTED_BACKGROUND='#282A2E'
NORMAL_FOREGROUND='#C5C8C6'
SELECTED_FOREGROUND='#81A2BE'
PROMPT='$'
CASE_INSENSITIVE=true
# LINES=49

dmenucmd="dmenu -fn -*-terminus-medium-*-*-*-16-*-*-*-*-*-*-*\
    -nb $NORMAL_BACKGROUND -sb $SELECTED_BACKGROUND \
    -nf $NORMAL_FOREGROUND -sf $SELECTED_FOREGROUND \
    -p $PROMPT -i" #-l $LINES

for arg in "$*"
do
    dmenucmd="$dmenucmd $arg"
done

$($dmenucmd)
