# ~/.zshrc

if zrcautoload colors && colors 2>/dev/null ; then
    BLUE="%{${fg[blue]}%}"
    RED="%{${fg_bold[red]}%}"
    GREEN="%{${fg[green]}%}"
    CYAN="%{${fg[cyan]}%}"
    MAGENTA="%{${fg[magenta]}%}"
    YELLOW="%{${fg[yellow]}%}"
    WHITE="%{${fg[white]}%}"
    NO_COLOR="%{${reset_color}%}"
else
    BLUE=$'%{\e[1;34m%}'
    RED=$'%{\e[1;31m%}'
    GREEN=$'%{\e[1;32m%}'
    CYAN=$'%{\e[1;36m%}'
    WHITE=$'%{\e[1;37m%}'
    MAGENTA=$'%{\e[1;35m%}'
    YELLOW=$'%{\e[1;33m%}'
    NO_COLOR=$'%{\e[0m%}'
fi

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
export BROWSER="firefox"
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

# irc
alias irc=weechat-curses

# Run urxvt via my script.
alias urxvt="/home/nathan/bin/urxvt.sh"

# less is more.
alias more='less'

# ls
alias ls='ls --color=auto'

# abbreviated listing
alias la='ls -a --color=auto'

# verbose listing
alias ll='ls -l --color=auto'

# sort by extension
alias lx='ll -BX --color=auto'

# sort by size
alias lz='ll -rS --color=auto'

# sort by date
alias lt='ll -rt --color=auto'

# cd
alias ..="cd .."
alias ...="cd ../.."

# git
alias gitps="git push origin master"
alias gitp="git pull"
alias gitc="git commit -a"
alias gitm="git commit "
alias giti="git init"
alias dotfiles='git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'

# chown
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

# Brightness control settings.
alias brightup='xbacklight -inc 20'
alias brightdown='xbacklight -dec 20'

# Rotate my wacom tablet.
# Make this automatic in the future!
alias wacom='xsetwacom --set "Wacom Intuos4 6x9 stylus" rotate half && xsetwacom --set "Wacom Intuos4 6x9 cursor" rotate half && xsetwacom --set "Wacom Intuos4 6x9 eraser" rotate half'

# Touchpad on/off.
alias touchon='synclient TouchpadOff=0'
alias touchoff='synclient TouchpadOff=1'

# blanking 
alias blankoff='xset -dpms; xset s off'
alias blanknow='xset dpms force off'

# Suspend my computer.
alias suspend='systemctl suspend'

# Window pager
#alias pager='~/bin/wmpager.sh && tail -f /tmp/wmpager.sh.pipe | dzen2 -ta l -x 0 -y 0 -w 153 -fn Terminus-8 -bg #002b36 -fg #657b83'

#alias czen='conky -c ~/.conkytoprc | dzen2 -ta l -x 153 -fn Terminus-8 -bg #002b36 -fg #657b83'

# give skype its own uers account
alias skype='xhost +local: && sudo -u skype /usr/bin/skype'
# start mpd and scmpc
alias pms='pms -p 6606'

# color my searches
alias grep='grep --color=auto'

alias sshorange='~/bin/sshorange.sh'

# view disowned files in pacman. Dangerous!
alias pacman-disowned='/home/nathan/bin/pacman-disowned'

# start eclim daemon
#alias eclimd = "/usr/share/eclipse/eclimd -Dosgi.instance.area.default=@nathan.home/jeclipse"
# Firefox addon SDK shortcut.
alias addon-sdk="cd /opt/addon-sdk && source bin/activate; cd -"

#eclim
alias eclim='/usr/share/eclipse/eclimd -Dnailgun.server.port=9091'

# file extension handling
alias -s tex=vim
alias -s conf=vim

# Append all history to this file
setopt appendhistory
setopt autocd
unsetopt beep nomatch notify
bindkey -v
# In grml's zshrc, this enables vi mode instead of emacs by default.
setopt vi
source ~/bin/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# better grml prompt
zstyle ':prompt:grml:*:percent' token '§ '
zstyle ':prompt:grml:*:percent' pre '%B%F{blue}'
zstyle ':prompt:grml:*:percent' post '%f%b'
zstyle ':prompt:grml:*:user' pre '%F{green}'
zstyle ':prompt:grml:*:user' post '%f'
zstyle ':prompt:grml:*:host' pre '%F{blue}'
zstyle ':prompt:grml:*:host' post '%f'
zstyle ':prompt:grml:*:at' token '@'
zstyle ':prompt:grml:*:at' pre '%f'
zstyle ':prompt:grml:*:at' post '%f'
zstyle ':prompt:grml:*:sad-smiley' token '%(?..×)'
zstyle ':prompt:grml:*:sad-smiley' pre '%B%F{red}'
zstyle ':prompt:grml:*:sad-smiley' post '%f%b'
zstyle ':prompt:grml:*:path' token '%40<..<%~%<< '
zstyle ':prompt:grml:*:path' pre '%F{yellow}%B'
zstyle ':prompt:grml:*:path' post '%f%b' 

# Change vcs_info formats for the grml prompt. The 2nd format sets up
# $vcs_info_msg_1_ to contain "zsh: repo-name" used to set our screen title.
# TODO: The included vcs_info() version still uses $VCS_INFO_message_N_.
#       That needs to be the use of $VCS_INFO_message_N_ needs to be changed
#       to $vcs_info_msg_N_ as soon as we use the included version.
# these are the same, just with a lot of colors:

    zstyle ':vcs_info:*' actionformats "${MAGENTA}${NO_COLOR}%s${MAGENTA}${YELLOW}/${MAGENTA}${GREEN}%b${YELLOW}|${RED}%a${MAGENTA}${NO_COLOR} " \
                                       "zsh: %r"
    zstyle ':vcs_info:*' formats       "${MAGENTA}${NO_COLOR}%s${MAGENTA}${YELLOW}/${MAGENTA}${GREEN}%b${MAGENTA}${NO_COLOR}%} " \
                                       "zsh: %r"
    zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat "%b${RED}:${YELLOW}%r"

# Show active directory in title bar.
# chpwd() {
#   [[ -o interactive ]] || return
#   case $TERM in
#     sun-cmd) print -Pn "\e]l%~\e\\"
#       ;;
#     *xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;%~\a"
#       ;;
#   esac
# }

mktar() { tar cvf "${1%%/}.tar" "${1%%/}/"; }
mktgz() { tar cvzf "${1%%/}.tar.gz" "${1%%/}/"; }
mktbz() { tar cvjf "${1%%/}.tar.bz2" "${1%%/}/"; }

# Maps a whole subnet with nmap
nnet () {
    if [[ -n $1 ]] ; then
        sudo nmap -sS -P0 $1
    fi
}

# File extract from somewhere on the Internet.
extract () {
   if [ -f $1 ] ; then
       case $1 in
	*.tar.bz2)	tar xvjf $1 && cd $(basename "$1" .tar.bz2) ;;
	*.tar.gz)	tar xvzf $1 && cd $(basename "$1" .tar.gz) ;;
	*.tar.xz)	tar Jxvf $1 && cd $(basename "$1" .tar.xz) ;;
	*.bz2)		bunzip2 $1 && cd $(basename "$1" /bz2) ;;
	*.rar)		unrar x $1 && cd $(basename "$1" .rar) ;;
	*.gz)		gunzip $1 && cd $(basename "$1" .gz) ;;
	*.tar)		tar xvf $1 && cd $(basename "$1" .tar) ;;
	*.tbz2)		tar xvjf $1 && cd $(basename "$1" .tbz2) ;;
	*.tgz)		tar xvzf $1 && cd $(basename "$1" .tgz) ;;
	*.zip)		unzip $1 && cd $(basename "$1" .zip) ;;
	*.Z)		uncompress $1 && cd $(basename "$1" .Z) ;;
	*.7z)		7z x $1 && cd $(basename "$1" .7z) ;;
	*)		echo "don't know how to extract '$1'..." ;;
       esac
   else
       echo "'$1' is not a valid file!"
   fi
}

absend () {
    if [ -f $1 ] ; then
        adb push $1 /sdcard/Books
    else
        echo "'$1' is not a valid file!"
    fi
}

absendall () {
    for file in `ls`
    do
        adb push $file /sdcard/Books
    done >& results.out
}

# colored man pages
man() {
    env LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	LESS_TERMCAP_md=$(printf "\e[1;31m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_se=$(printf "\e[0m") \
	LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[1;32m") \
	man "$@"
}

