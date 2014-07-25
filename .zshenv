typeset -U path

export RBENV_VERSION=2.1.2
# ensure all terminal emulators use the right shell
export SHELL='/bin/zsh'

export HISTFILE=$HOME/.zhistory
# The number of commands stored in memory
export HISTSIZE=1000
# The number of commands saved in my history file
export SAVEHIST=1000

# pager settings (grml)
export PAGER=${PAGER:-less}
export LESS="-R"
# support colors in less (grml)
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3

# mail (grml)
export MAIL=${MAIL:-/var/mail/$USER}
# mailchecks (grml)
MAILCHECK=30

# report about cpu-/system-/user-time of command if running longer than
# 5 seconds
REPORTTIME=5

# Export surfraw's conf files since it's too dumb to find them
export SURFRAW_conf="$XDG_CONFIG_HOME/surfraw/conf"

export EDITOR="$HOME/bin/ec"
export SUDO_EDITOR="rvim"
export BROWSER="firefox"
export GREP_COLOR="1;33"

# gtk style for qt5
export QT_STYLE_OVERRIDE="gtk"

export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$HOME/bin:$PATH"
# Rust in $PATH
RUSTBIN=/home/nathan/devel/rust/rust/x86_64-unknown-linux-gnu/stage2/bin
which rustc &> /dev/null
if [[ -e $RUSTBIN ]]; then
    path+=($RUSTBIN)
    if [[ -z $(echo $LD_LIBRARY_PATH | grep "rust/x86_64-unknown-linux-gnu") ]]; then
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$HOME/devel/rust/rust/x86_64-unknown-linux-gnu/stage2/lib"
    fi
fi

# cabal!
CABALPATH=$HOME/.cabal/bin
echo "$PATH" | grep "\.cabal/bin" &> /dev/null
if [[ "$?" -eq "1" && -e $CABALPATH ]]; then
    export PATH=$CABALPATH:$PATH
fi

# Steam likes this
export SDL_AUDIODRIVER=alsa

export XDG_CONFIG_HOME='/home/nathan/.config'

CARGOPATH="$HOME/devel/rust/cargo/target"
echo "$PATH" | grep "rust/cargo/target" &> /dev/null
if [[ "$?" -eq "1" && -e $CARGOPATH ]]; then
    export PATH=$PATH:$CARGOPATH
fi

NODEBIN="$HOME/node_modules/.bin"
echo "$PATH" | grep "$NODEBIN" &> /dev/null
if [[ "$?" -eq "1" && -e $NODEBIN ]]; then
    export PATH=$PATH:$NODEBIN
fi

# ccache
dpkg-query -l "ccache" &> /dev/null
if [ "$?" -eq "0" ]; then
    export CCACHE_DIR=$HOME/.ccache
fi

# place custom scripts in path, taking precedece over the other binaries
#path+=("/home/nathan/bin")

# Fixes swing stuff in xmonad
export _JAVA_AWT_WM_NONREPARENTING=1
path+=(/home/nathan/devel/java/android-sdk-linux/tools)
