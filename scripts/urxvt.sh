#!/bin/bash
#
# This script starts urxvt.
#

#urxvt -fn 'xft:Terminus-9' -e bash -c "tmux -q has-session && exec tmux attach-session -d || exec tmux new-session -n$USER -s$USER@$HOSTNAME"
urxvt -fn 'xft:Terminus-9' &
