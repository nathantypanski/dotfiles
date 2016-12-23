#!/bin/bash

if [[ -z "${HOME}" ]]; then
    2>&1 echo "No ${HOME} directory. What the fuck?"
    exit 1
fi

WINDOW_MANAGER='i3'

XRESOURCES="${HOME}/.xresources"
XMODMAPRC="${HOME}/.xmodmaprc"
XBINDKEYS="${HOME}/.xbindkeysrc.scm"

which setxkbmap
if [[ $? -eq 0 ]]; then
    setxkbmap -layout us -option ctrl:nocaps 
fi

which xbindkeys
if [[ $? -eq 0 ]]; then
    xbindkeys
fi
 
which xrdb
if [[ $? -eq 0 && -f "${XRESOURCES}" ]]; then
    xrdb -merge "${XRESOURCES}"
fi
which xmodmap
if [[ $? -eq 0 && -f "${XMODMAPRC}" ]]; then
    xmodmap "${XMODMAPRC}"
fi
which xbindkeys
if [[ $? -eq 0 && -f "${XBINDKEYS}" ]]; then
    xbindkeys -file "${XBINDKEYS}"
fi

exec "${WINDOW_MANAGER}"
