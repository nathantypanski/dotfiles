#!/bin/bash

if [ -n "$XDG_CONFIG_HOME" ]; then
    XDG_CONFIG_HOME="$HOME"'/.config'
fi

if [ `uname` = 'Darwin' ]; then
    OSX=true
fi

# These commands are dangerous if this is unbound.
[[ -z "$HOME" ]] && exit 1

I1='^.dotfiles/?.*'
I2='.*README.md'
I3='.*LICENSE$'

stow -v --ignore="$I1" --no-folding --target="$XDG_CONFIG_HOME"'/ranger' 'ranger'
stow -v --ignore="$I1" --no-folding --target="$HOME"'/bin' 'bin'

stow -v --ignore="$I1" --no-folding --target="$HOME" 'zsh'
stow -v --ignore="$I1" --no-folding --target="$HOME" 'tmux'
stow -v --ignore="$I1" --no-folding --target="$HOME" 'vim'
stow -v --ignore="$I1" --target="$HOME" 'emacs'

if [ "$OSX" != true ]; then
    # Linux-only stuff below
    stow -v --ignore="$I1" --target="$XDG_CONFIG_HOME"'/systemd' 'systemd'
    stow -v --ignore="$I1" --no-folding --target="$XDG_CONFIG_HOME" 'i3'
    stow -v --ignore="$I1" --no-folding --target="$XDG_CONFIG_HOME" 'herbstluftwm'
    stow -v --ignore="$I1" --no-folding --target="$HOME" 'mutt'
    stow -v --ignore="$I1" --no-folding --target="$HOME" 'xresources'
    stow -v --ignore="$I1" --target="$HOME" 'xmonad'
    stow -v --ignore="$I1" --no-folding --target="$HOME" 'xbindkeys'
    stow -v --ignore="$I1" --no-folding --target="$HOME" 'xmodmap'
    stow -v --ignore="$I1" --no-folding --target="$HOME/.weechat" 'weechat'
    # individual script links
    ln -s $(pwd)'/displaylink.sh/displaylink.sh' "$HOME"'/bin' 
    ln -s $(pwd)'cabver/cabver.py' "$HOME"'/bin' 
fi
