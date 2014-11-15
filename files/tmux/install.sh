#!/usr/bin/bash

# Install my tmux config.

BACKUP='numbered'

ln -sri --backup="$BACKUP" './tmux.conf' "$HOME"'/.tmux.conf'
