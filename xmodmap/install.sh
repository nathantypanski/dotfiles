#!/usr/bin/bash

# Install xmodmap configuration.

BACKUP='numbered'

ln -sri --backup="$BACKUP" './xmodmap' "$HOME"'/.xmodmap'
