#!/usr/bin/bash

# Install my vim config.

ln -sri --backup="$BACKUP" './vimrc' "$HOME"'/.vimrc'
