# android sdk exports
/etc/profile.d/android-sdk.sh

# ensure all terminal emulators use the right shell
export SHELL='/bin/zsh'

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
# 5 seconds (grml)
REPORTTIME=5

# perl
export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:/home/nathan/perl5";
export PERL_MB_OPT="--install_base /home/nathan/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/nathan/perl5";
export PERL5LIB="/home/nathan/perl5/lib/perl5:$PERL5LIB";
export PATH="/home/nathan/perl5/bin:$PATH";
# powerline
#export POWERLINE_CONFIG_PATH="/home/nathan/.config/powerline"
export HISTFILE=~/.zhistory
# The number of commands stored in memory
export HISTSIZE=1000
# The number of commands saved in my history file
export SAVEHIST=1000

# Vim for life.
export EDITOR="$HOME/bin/ec"
EDITOR="emacsclient -t" # text-mode client, connects to running daemon
export SUDO_EDITOR="rvim"

export BROWSER="firefox"

export GREP_COLOR="1;33"


export CLASSPATH="/usr/share/java/junit.jar"
#export PYTHONPATH=/usr/lib/python2.7/site-packages:/usr/lib/python3.3/site-packages:~/bin/i3-py
export PYTHONPATH=/usr/lib/python3.3/site-packages:~/bin/i3-py
# scripts in path
export PATH=$PATH:~/bin:~/.gem/ruby/2.0.0/bin:~/.cabal/bin:/opt/java/jre/bin
export TERMCMD=urxvt
# Steam likes this
export SDL_AUDIODRIVER=alsa
# Eclipse doesn't seem to play nice by default.
export ECLIPSE_HOME="/usr/share/eclipse"
#syncad
export SYNCAD_LICENSE_FILE=~/synapticad-17.07d/license.dat
export CCACHE_DIR=~/.ccache
export CHROOT=$HOME/chroot
export XDG_CONFIG_HOME='/home/nathan/.config'

# bspwm
export PATH=$PATH:$ANDROID_HOME/platform-tools
export BSPWM_TREE=/tmp/bspwm.tree
export BSPWM_HISTORY=/tmp/bspwm.history
export BSPWM_STACK=/tmp/bspwm.stack
