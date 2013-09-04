# perl
export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:/home/nathan/perl5";
export PERL_MB_OPT="--install_base /home/nathan/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/nathan/perl5";
export PERL5LIB="/home/nathan/perl5/lib/perl5:$PERL5LIB";
export PATH="/home/nathan/perl5/bin:$PATH";
# powerline
export POWERLINE_CONFIG_PATH="/home/nathan/.config/powerline"
export HISTFILE=~/.zhistory
# The number of commands stored in memory
export HISTSIZE=1000
# The number of commands saved in my history file
export SAVEHIST=1000
# Vim for life.
export EDITOR="vim"
export SUDO_EDITOR="rvim"
# Since there's nothing better out there ...
export BROWSER="dwb"
export GREP_COLOR="1;33"
export LESS="-R"
export CLASSPATH="/usr/share/java/junit.jar"
#export PYTHONPATH=/usr/lib/python2.7/site-packages:/usr/lib/python3.3/site-packages:~/bin/i3-py
export PYTHONPATH=/usr/lib/python3.3/site-packages:~/bin/i3-py
# scripts in path
export PATH=$PATH:~/bin:~/.gem/ruby/2.0.0/bin:~/.cabal/bin:/opt/java/jre/bin
export TERM_PROGRAM=$TERM
export TERMCMD=termite
# Steam likes this
export SDL_AUDIODRIVER=alsa
# Eclipse doesn't seem to play nice by default.
export ECLIPSE_HOME="/usr/share/eclipse"
#syncad
export SYNCAD_LICENSE_FILE=~/synapticad-17.07d/license.dat
export CCACHE_DIR=~/.ccache
export CHROOT=$HOME/chroot
export XDG_CONFIG_HOME='/home/nathan/.config'
