#!/bin/sh

# Just runs dmenu, with my settings.

dmenu -fn '-*-terminus-medium-*-*-*-16-*-*-*-*-*-iso8859-*'\
    -nb '#002b36' -sb '#073642' -nf '#839496' -sf '#859900' "$@" -p "$"
