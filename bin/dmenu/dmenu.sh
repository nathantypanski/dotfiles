#!/bin/bash

# Just runs dmenu, with my settings.

NORMAL_BACKGROUND='#1D1F21'
SELECTED_BACKGROUND='#282A2E'
NORMAL_FOREGROUND='#C5C8C6'
SELECTED_FOREGROUND='#81A2BE'
PROMPT='$'
CASE_INSENSITIVE=true
# LINES=49

# For whatever reason, my Ubuntu machine at work won't accept X logical font
# descriptions.
if [[ "$(dmenu -v)" == 'dmenu-4.6' ]]; then
    FONT='Terminus'
else
    FONT='-*-terminus-medium-*-*-*-16-*-*-*-*-*-iso8859-*'
fi
dmenucmd="dmenu -fn $FONT \
    -nb $NORMAL_BACKGROUND -sb $SELECTED_BACKGROUND \
    -nf $NORMAL_FOREGROUND -sf $SELECTED_FOREGROUND \
    -p $PROMPT" #-l "$LINES" "$@"

`$dmenucmd`
