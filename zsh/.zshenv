typeset -U path

export SHELL='/bin/zsh'
export HISTFILE=$HOME'/.zhistory'
# The number of commands stored in memory
export HISTSIZE=1000000
# The number of commands saved in my history file
export SAVEHIST=1000000

export XDG_CONFIG_HOME="$HOME"'/.config'
export XDG_DATA_HOME="$HOME"'/.local/share'

# pager settings (grml)
export PAGER='less'
export LESS="-R"

# support colors in less (grml)
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# My default switches for ls
export LS_LL_DEFAULT_SWITCHES='-lhkF'
export LS_DEFAULT_SWITCHES='--color=auto'

# gtags
export GTAGSLIBPATH="$XDG_DATA_HOME"'/gtags'

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

# Steam likes this
export SDL_AUDIODRIVER=alsa

# ccache
export CCACHE_DIR=$HOME/.ccache

# Fixes swing stuff in xmonad
export _JAVA_AWT_WM_NONREPARENTING=1

typeset -TUx PYTHONPATH pythonpath 
pythonpath+=("$HOME"'/prj/ndtpy/src')

# golang
export GOPATH="$HOME"/prj/go
export GOBIN="$GOPATH"/bin
path=("$GOBIN" "$path[@]")
export CM_ROOT="$GOPATH/src/github.com/10gen/mms-automation/go_planner"

path=("$HOME"'/.local/bin'                         "$path[@]")
path=("$HOME"'/.cabal/bin'                         "$path[@]")
path=("$HOME"'/node_modules/.bin'                  "$path[@]")
path=("$HOME"'/devel/go/bin'                       "$path[@]")
path=("$HOME"'/bin'                                "$path[@]")
path=("$HOME"'/.rbenv/bin'                         "$path[@]")
path=("$HOME"'/.chefdk/gem/ruby/2.1.0/bin'         "$path[@]")
if [[ -f "${HOME}/.cargo/env" ]]; then
    source "${HOME}/.cargo/env"
fi
path=("$HOME"'/.chefdk/gem/ruby/2.1.0/bin'         "$path[@]")

if [ `uname` = 'Darwin' ]; then
    # OSX
    export VIRTUALENVWRAPPER_PYTHON='/usr/bin/python'
    path=('/usr/local/bin' "$path[@]")
    path=('/usr/local/opt/coreutils/libexec/gnubin'    "$path[@]")
    path=("$HOME/.jenv/bin:$PATH" "$path[@]")
    MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
    eval "$(jenv init -)"
    PY27BIN="$HOME/Library/python/2.7/bin"
    if [ -d "$PY27BIN" ]; then
        path=("$PY27BIN" "$path[@]")
    fi
    if [ -f "$HOME/.HOMEBREW_GITHUB_API_TOKEN" ]; then
        export HOMEBREW_GITHUB_API_TOKEN="$(cat ~/.HOMEBREW_GITHUB_API_TOKEN)"
    fi
    path=("$HOME/Library/Haskell/bin" "$path[@]")
    path=("$HOME/.gem/ruby/2.3.0/bin" "$path[@]")
    export JAVA_HOME=$(/usr/libexec/java_home)
else
    # Linux
    export VIRTUALENVWRAPPER_PYTHON='/usr/bin/python'
    path=("$HOME"'/.rbenv/bin'                         "$path[@]")
    path=("$HOME"'/.xmonad/.cabal-sandbox/bin' "$path[@]")
    path=('/usr/lib/ccache/bin'                      "$path[@]")
    path=('/opt/android-sdk/platform-tools'         "$path[@]")
    if [ -d "$HOME"/npm/bin ]; then
        path=("$HOME"'/npm/bin' "$path[@]")
    fi
fi


EC2KEYFILE="$HOME"'/.config/zsh/ec2.zsh'
if [ -f "$EC2KEYFILE" ]; then
    source "$EC2KEYFILE"
fi
