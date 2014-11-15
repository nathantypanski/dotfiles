#!/usr/bin/bash

# Install xbindkeys configuration.

BACKUP='numbered'

ln -sri --backup="$BACKUP" './xbindkeysrc.scm' "$HOME"'/.xbindkeysrc.scm'
