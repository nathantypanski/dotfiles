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

# mail (grml)
export MAIL=${MAIL:-/var/mail/$USER}
# mailchecks (grml)
MAILCHECK=30

# report about cpu-/system-/user-time of command if running longer than
# 5 seconds
REPORTTIME=5

export EDITOR="$HOME/bin/ec"
export SUDO_EDITOR="rvim"
export BROWSER="firefox"
export GREP_COLOR="1;33"

# gtk style for qt5
export QT_STYLE_OVERRIDE="gtk"

# Ruby gems
RUBYGEMDIR=$HOME"/.gem/ruby/2.0.0/bin"
if [ -e $RUBYGEMDIR ]; then
    export GEM_HOME=$HOME/.gem
    export GEM_PATH=$GEM_PATH:$HOME/.gem
    export PATH=$PATH:$RUBYGEMDIR
fi

# Rust in $PATH
RUSTBIN=/home/nathan/devel/rust/rust/x86_64-unknown-linux-gnu/stage2/bin
which rustc &> /dev/null
if [[ "$?" -eq "1" && -e $RUSTBIN ]]; then
    export PATH=$PATH:$RUSTBIN
fi

# ghc
GHCPATH=$HOME/.ghc/bin
which ghc &> /dev/null
if [[ "$?" -eq "1" && -e $GHCPATH ]]; then
    export PATH=$PATH:$GHCPATH
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

# ccache
dpkg-query -l "ccache" &> /dev/null
if [ "$?" -eq "0" ]; then
    export CCACHE_DIR=$HOME/.ccache
fi

# place custom scripts in path, taking precedece over the other binaries
echo $PATH | grep "$HOME/bin" &> /dev/null
if [[ "$?" -eq "1" && -e "$HOME/bin" ]]; then
    export PATH=$HOME/bin:$PATH
fi
