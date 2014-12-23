#!/usr/bin/bash

if [ -n "$XDG_CONFIG_HOME" ]; then
    XDG_CONFIG_HOME="$HOME"'/.config'
fi

IGNORE='^.dotfiles/?.*'

stow -v --ignore="$IGNORE" --target="$XDG_CONFIG_HOME"'/systemd' 'systemd'
stow -v --ignore="$IGNORE" --no-folding --target="$XDG_CONFIG_HOME"'/ranger' 'ranger'
stow -v --ignore="$IGNORE" --no-folding --target="$XDG_CONFIG_HOME" 'i3'
stow -v --ignore="$IGNORE" --no-folding --target="$XDG_CONFIG_HOME" 'herbstluftwm'
stow -v --ignore="$IGNORE" --no-folding --target="$HOME"'/bin' 'bin'

stow -v --ignore="$IGNORE" --no-folding --target="$HOME" 'mutt'
stow -v --ignore="$IGNORE" --no-folding --target="$HOME" 'zsh'
stow -v --ignore="$IGNORE" --no-folding --target="$HOME" 'xresources'
stow -v --ignore="$IGNORE" --no-folding --target="$HOME" 'tmux'
stow -v --ignore="$IGNORE" --no-folding --target="$HOME" 'vim'
stow -v --ignore="$IGNORE" --target="$HOME" 'emacs'
stow -v --ignore="$IGNORE" --target="$HOME" 'xmonad'
stow -v --ignore="$IGNORE" --no-folding --target="$HOME" 'xbindkeys'
stow -v --ignore="$IGNORE" --no-folding --target="$HOME/.weechat" 'weechat'

# individual script links
ln -s $(pwd)'/displaylink.sh/displaylink.sh' "$HOME"'/bin' 
ln -s $(pwd)'cabver/cabver.py' "$HOME"'/bin' 
