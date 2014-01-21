# android sdk exports
#/etc/profile.d/android-sdk.sh

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

# Load i3wm scripts, iff binary i3 is in PATH.
i3SCRIPTDIR="$HOME/bin/i3-py"
which i3 &> /dev/null
if [[ "$?" -eq "0" && -e $i3SCRIPDIR ]]; then
    export PYTHONPATH=$PYTHONPATH:$i3SCRIPTDIR
fi

# Ruby gems
RUBYGEMDIR=$HOME"/.gem/ruby/2.0.0/bin"
if [ -e $RUBYGEMDIR ]; then
    export PATH=$PATH:$RUBYGEMDIR
fi

# cabal!
CABALPATH=$HOME/.cabal/bin
which ghc &> /dev/null
if [[ "$?" -eq "0" && -e $CABALPATH ]]; then
    export PATH=$PATH:$CABALPATH
fi

which urxvt &> /dev/null
if [ "$?" -eq "0" ]; then
    export TERMCMD=urxvt
fi

# Steam likes this
export SDL_AUDIODRIVER=alsa

export XDG_CONFIG_HOME='/home/nathan/.config'

# ccache
dpkg-query -l "ccache" &> /dev/null
if [ "$?" -eq "0" ]; then
    export CCACHE_DIR=$HOME/.ccache
fi

# bspwm
which bspwm &> /dev/null
if [ "$?" -eq "0" ]; then
    export PATH=$PATH:$ANDROID_HOME/platform-tools
    export BSPWM_TREE=/tmp/bspwm.tree
    export BSPWM_HISTORY=/tmp/bspwm.history
    export BSPWM_STACK=/tmp/bspwm.stack
fi

# fix java fonts
which java &> /dev/null
if [ "$?" -eq "0" ]; then
    export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true'
    export CLASSPATH="/usr/share/java/junit.jar"
    export ECLIPSE_HOME="/usr/share/eclipse"
fi

# Is Chrome installed?
dpkg-query -l "google-chrome-stable" &> /dev/null
if [ "$?" -eq "0" ]; then
    export PATH=$PATH:"/opt/google/chrome"
fi

# place custom scripts in path, taking precedece over the other binaries
if [ -e "$HOME/bin" ]; then
    export PATH=$HOME/bin:$PATH
fi
