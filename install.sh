#!/usr/bin/bash

if [ -n "$XDG_CONFIG_HOME" ]; then
    XDG_CONFIG_HOME="$HOME"'/.config'
fi

stow -v --target="$XDG_CONFIG_HOME" systemd
stow -v --no-folding --target="$XDG_CONFIG_HOME/ranger" ranger
stow -v --no-folding --target="$XDG_CONFIG_HOME" i3
stow -v --no-folding --target="$XDG_CONFIG_HOME" herbstluftwm
stow -v --no-folding --target="$HOME/bin" bin
stow -v --no-folding --target="$HOME" mutt
stow -v --no-folding --target="$HOME" zsh
stow -v --no-folding --target="$HOME" xresources
stow -v --no-folding --target="$HOME" tmux
stow -v --no-folding --target="$HOME" vim
stow -v --target="$HOME" emacs
stow -v --target="$HOME" xmonad
stow -v --no-folding --target="$HOME" xbindkeys
stow -v --no-folding --target="$HOME/.weechat" weechat

