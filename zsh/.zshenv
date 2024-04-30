typeset -U path
set +x

umask 022

check_and_add_to_path() {
    # Check that a directory exists, then add it to the start of the path.
    #
    # Args:
    #
    # $1 - path to add
    #
    # Returns: 0 on success, 1 on failure.
    MAYBE_PATH="${1}"
    if [[ -d "${MAYBE_PATH}" ]]; then
        path=("${MAYBE_PATH}" "$path[@]")
        return 0
    fi
    return 1
}

append_path() {
    check_and_add_to_path $@
}

for f in /etc/profile.d/*.sh; do
    . "${f}"
done

check_and_source() {
    # Check that a file exists, then source it.
    #
    # Args:
    #
    # $1 - file to source
    #
    # Returns: 0 on success, 1 on failure.
    MAYBE_PATH="${1}"
    if [[ -f "${MAYBE_PATH}" ]]; then
        source "${MAYBE_PATH}"
        return 0
    fi
    return 1
}


export SHELL='/bin/zsh'
export HISTFILE="${HOME}/.zhistory"
# The number of commands stored in memory
export HISTSIZE=1000000
export HISTCONTROL=ignorespace
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

# ccache
export CCACHE_DIR=$HOME/.ccache

# Fixes swing stuff in xmonad
export _JAVA_AWT_WM_NONREPARENTING=1

typeset -TUx PYTHONPATH pythonpath 
pythonpath+=("$HOME"'/prj/ndtpy/src')

# golang
export GOPATH="$HOME"/prj/go
export GOBIN="$GOPATH"/bin
check_and_add_to_path "${GOBIN}"
export CM_ROOT="$GOPATH/src/github.com/10gen/mms-automation/go_planner"

path=("$HOME"'/.local/bin'                         "$path[@]")
path=("$HOME"'/.cabal/bin'                         "$path[@]")
path=("$HOME"'/node_modules/.bin'                  "$path[@]")
path=("$HOME"'/bin'                                "$path[@]")
check_and_add_to_path "${HOME}/.rbenv/bin"
check_and_add_to_path "${HOME}/.chefdk/gem/ruby/2.3.0/bin"
CARGO_ENV="${HOME}/.cargo/env"
check_and_source "${CARGO_ENV}"

if [ "$(uname)" = 'Darwin' ]; then
    export DARWIN="${OSTYPE}"
    # OSX
    #
    # First up: Before using this we want to disable /etc/paths by moving it
    # somewhere else:
    #
    #    # mv /etc/paths /etc/paths.bak
    #
    # Now we set our path variables and do the rest of our OS X specific magic.
    path=('/usr/bin' "$path[@]")
    path=('/bin' "$path[@]")
    path=('/usr/sbin' "$path[@]")
    path=('/sbin' "$path[@]")

    export VIRTUALENVWRAPPER_PYTHON='/usr/bin/python'
    path=('/usr/local/bin' "$path[@]")
    path=('/usr/local/opt/coreutils/libexec/gnubin' "$path[@]")

    JENV_BIN="${HOME}/.jenv/bin"
    check_and_add_to_path "${JENV_BIN}"
    if [[ -d  "${JENV_BIN}" ]]; then
        eval "$(jenv init -)"
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi

    GNU_COREUTILS='/opt/local/libexec/gnubin'
    check_and_add_to_path "${GNU_COREUTILS}"
    if [[ $? -eq 0 ]]; then
        path=("${GNU_COREUTILS}" "${path[@]}")
        MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
        # Since we're using GNU coreutils, we can use `--color=auto`
        export LS_DEFAULT_SWITCHES='--color=auto'
    fi
    if [[ -d "/opt/local/bin" ]]; then
        path=("/opt/local/bin" "${path[@]}")
    fi
    if [ -f "$HOME/.HOMEBREW_GITHUB_API_TOKEN" ]; then
        export HOMEBREW_GITHUB_API_TOKEN="$(cat ~/.HOMEBREW_GITHUB_API_TOKEN)"
    fi
else
    # Linux
    export LS_DEFAULT_SWITCHES='--color=auto'
    export VIRTUALENVWRAPPER_PYTHON='/usr/bin/python'

    # Steam likes this
    export SDL_AUDIODRIVER=alsa
    check_and_add_to_path "${HOME}/devel/go/bin"
    check_and_add_to_path "${HOME}/devel/go/bin"
    check_and_add_to_path "$HOME/.xmonad/.cabal-sandbox/bin"
    check_and_add_to_path '/usr/lib/ccache/bin' 
    check_and_add_to_path '/opt/android-sdk/platform-tools'
    check_and_add_to_path "${HOME}/npm/bin"
    check_and_add_to_path "${HOME}/.cargo/bin"
fi

EC2KEYFILE="$HOME"'/.config/zsh/ec2.zsh'
if [ -f "$EC2KEYFILE" ]; then
    source "$EC2KEYFILE"
fi

export RUST_LOG=error

. /opt/asdf-vm/asdf.sh
