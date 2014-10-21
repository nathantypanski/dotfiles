typeset -U path

export RBENV_VERSION=2.1.2
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

# Export surfraw's conf files since it's too dumb to find them
export SURFRAW_conf="$XDG_CONFIG_HOME/surfraw/conf"

export EDITOR="$HOME/bin/ec"
export SUDO_EDITOR="rvim"
export BROWSER="firefox"
export GREP_COLOR="1;33"

# gtk style for qt5
export QT_STYLE_OVERRIDE="gtk"


# cabal!
# Steam likes this
export SDL_AUDIODRIVER=alsa

export XDG_CONFIG_HOME='/home/nathan/.config'
export GOPATH="$HOME"/prj/go
export GOBIN="$GOPATH"/bin
path+=("$GOBIN")

export RUST_ROOT="$HOME"/prj/rust/rust/stage2

path+=("$HOME"/.rbenv/bin)
path+=("$RUST_ROOT"/bin)
path+=("$HOME"/devel/java/android-sdk-linux/tools)
path+=("$HOME"/.cabal/bin)
path+=("$HOME"/devel/rust/cargo/target)
path+=("$HOME"/node_modules/.bin)
path+=("$HOME"/devel/go/bin)
path+=("$HOME"/bin)

# ccache
#export CCACHE_DIR=$HOME/.ccache

# Fixes swing stuff in xmonad
export _JAVA_AWT_WM_NONREPARENTING=1

export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
if [ -n "$PYTHONPATH" ]; then
    export PYTHONPATH="$PYTHONPATH":"$HOME"/prj/ndtpy/src
else
    export PYTHONPATH="$HOME"/prj/ndtpy/src
fi

path=(/usr/lib/ccache/bin/ "$path[@]")
path+=("/opt/android-sdk/platform-tools")
path+=("/home/nathan/.xmonad/.cabal-sandbox/bin")
