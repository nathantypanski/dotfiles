#!/bin/bash

if [[ -z "${HOME}" ]]; then
    2>&1 echo "No ${HOME} directory. What the fuck?"
    exit 1
fi

fail() {
    if [[ $# -le 1 ]]; then
        >&2 echo "wrong args to fail()"
    fi

    >&2 echo $@
    exit 1
}

WINDOW_MANAGER='/usr/bin/i3'
# WINDOW_MANAGER='/home/nathan/prj/wtftw/target/release/wtftw'
# WINDOW_MANAGER='/home/nathan/prj/autiswm/target/debug/xr3wm'

if [[ ! -x "${WINDOW_MANAGER}" ]]; then
    fail "${WINDOW_MANAGER} is not executable"
fi

XRESOURCES="${HOME}/.Xresources"
XMODMAPRC="${HOME}/.xmodmaprc"
XBINDKEYS="${HOME}/.xbindkeysrc.scm"

if which setxkbmap &> /dev/null ; then
    setxkbmap -layout us -option ctrl:nocaps
    if [[ "$?" != 0 ]]; then
        fail "failed setxkbmap"
    fi
else
    >&2 echo "failed to load setxkbmap"
fi

if which xrdb &> /dev/null && [[ -r "${XRESOURCES}" ]]; then 
    xrdb -merge "${XRESOURCES}"
    if [[ "$?" != 0 ]]; then
        fail "failed to merge XRESOURCES"
    fi
else
    >&2 echo "failed to load xrdb"
    exit 1
fi

if which xmodmap &> /dev/null && [[ -r "${XMODMAPRC}" ]]; then
    xmodmap "${XMODMAPRC}"
    if [[ "$?" != 0 ]]; then
        >&2 echo "failed xmodmap"
    fi
else
    >&2 echo "failed to load xmodmap"
    exit 1
fi

if which xbindkeys &> /dev/null && [[ -r "${XBINDKEYS}" ]]; then
    xbindkeys -fg "${XBINKDEYS}"
    if [[ "$?" != 0 ]]; then
        fail "failed xbindkeys"
    fi
else
    >&2 echo "failed to load xbindkeys"
    exit 1
fi

if ! systemctl --user import-environment DISPLAY; then
    fail "failed to import environment DISPLAY"
fi

synclient PalmDetect=1
if [[ "$?" != '0' ]]; then
    fail "failed setting palm detection"
fi

# RUST_LOG=debug exec "${WINDOW_MANAGER}" >> "${HOME}/xr3wm.log" 2>> "${HOME}/xr3wm.err.log"
exec "${WINDOW_MANAGER}"
# exec /usr/bin/stumpwm
# exec /usr/bin/i3 -V &
# plz work
