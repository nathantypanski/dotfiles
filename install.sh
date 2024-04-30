#!/bin/bash

# constants
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# These commands are dangerous if this is unbound.
if [[ -z "${HOME}" ]]; then
    2>&1 echo '${HOME} must be bound to run this script'
    exit 1
fi

DISPLAYLINK_SCRIPT="${SCRIPT_DIR}/displaylink.sh/displaylink.sh"
CABVER="${SCRIPT_DIR}/cabver/cabver.py"
WEECHAT_HOME="${HOME}/.weechat"

# Short circuit if the user does not have stow
which stow &> /dev/null
if [[ $? -ne 0 ]]; then
    2>&1 echo "You must install GNU stow to use this installer."
    exit 1
fi

if [ -n "$XDG_CONFIG_HOME" ]; then
    XDG_CONFIG_HOME="${HOME}/.config"
fi

# Automatically create ~/bin directory if it does not exist
if [[ ! -d "${HOME}/bin" ]]; then
    mkdir "${HOME}/bin" 
fi

# Automatically create ~/.config directory if it does not exist
if [[ ! -d "${XDG_CONFIG_HOME}" ]]; then
    mkdir "${XDG_CONFIG_HOME}" 
fi

# Detect OS.
if [ $(uname) = 'Darwin' ]; then
    OSX='true'
    ARCH='false'
elif [[ -f '/etc/arch-release' ]]; then
    OSX='false'
    ARCH='true'
else
    OSX='false'
    ARCH='false'
fi

I1='^.dotfiles/?.*'
I2='.*README.md'
I3='.*LICENSE$'

stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="${HOME}"'/bin' 'bin'
stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="${HOME}" 'zsh'
stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="${HOME}" 'vim'
stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --target="${HOME}" 'emacs'

if [ "${ARCH}" == 'true' ]; then
    echo "Stowing arch"
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --target="$XDG_CONFIG_HOME"'/systemd' 'systemd'
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="$XDG_CONFIG_HOME" 'i3'
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="$XDG_CONFIG_HOME" 'herbstluftwm'
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="$HOME" 'mutt'
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --target="$HOME" 'xmonad'
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --target="$HOME" 'xinitrc'
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --target="$HOME" 'stumpwm'
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --target="${HOME}" 'sway'

    which xrdb &> /dev/null
    if [[ "$?" -eq 0 ]]; then
        stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="$HOME" 'xresources'
    fi

    which xbindkeys &> /dev/null
    if [[ "$?" -eq 0 ]]; then
        stow -v --ignore="$I1" --no-folding --target="$HOME" 'xbindkeys'
    fi

    which xmodmap &> /dev/null
    if [[ $? -eq 0 ]]; then
        stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="$HOME" 'xmodmap'
    fi

    which weechat &> /dev/null
    if [[ "$?" -eq 0 ]]; then
        # Automatically create ~/.weechat directory if it does not exist
        if [[ ! -d "${WEECHAT_HOME}" ]]; then
            mkdir "${WEECHAT_HOME}" 
        fi
        stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="$HOME/.weechat" 'weechat'
    fi
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="$HOME" 'termite'
    # individual script links
    if [[ -h "${DISPLAYLINK_SCRIPT}" ]]; then
        ln -s "${DISPLAYLINK_SCRIPT}" "${HOME}/bin"
    fi
    if [[ -h "${CABVER}" ]]; then
        ln -s "${CABVER}" "${HOME}/bin"
    fi
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="${HOME}" 'tmux'
elif [[ "${OSX}" = 'true' ]]; then
    DARWIN_MAJOR_VERSION="$(echo "${OSTYPE}"| sed 's/darwin//g' | tr '.' ' ' | cut -d' ' -f 1)"
    if [[ "${DARWIN_MAJOR_VERSION}" -eq 16 ]]; then
        stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="${HOME}" 'tmux.macos'
    fi
    stow -v --ignore="$I1" --ignore="$I2" --ignore="$I3" --no-folding --target="${HOME}" 'tmux'
fi
