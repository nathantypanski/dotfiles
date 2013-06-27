# ~/.zshrc
#
# This file uses grml's zshrc as a master, and builds upon that.
# Keep this in mind when working with these things.

# In grml's zshrc, this enables vi mode instead of emacs by default.
setopt vi

# better grml prompt
zstyle ':prompt:grml:*:percent' token '§ '
zstyle ':prompt:grml:*:percent' pre '%F{blue}'
zstyle ':prompt:grml:*:percent' post '%f'
zstyle ':prompt:grml:*:user' pre '%F{green}'
zstyle ':prompt:grml:*:user' post '%f'
zstyle ':prompt:grml:*:host' pre '%f'
zstyle ':prompt:grml:*:host' post '%f'
zstyle ':prompt:grml:*:at' token '@'
zstyle ':prompt:grml:*:at' pre '%f'
zstyle ':prompt:grml:*:at' post '%f'
zstyle ':prompt:grml:*:sad-smiley' token '%(?..×)'
zstyle ':prompt:grml:*:sad-smiley' pre '%B%F{red}'
zstyle ':prompt:grml:*:sad-smiley' post '%f%b'
zstyle ':prompt:grml:*:path' token '%40<..<%~%<< '
zstyle ':prompt:grml:*:path' pre '%F{blue}%B'
zstyle ':prompt:grml:*:path' post '%f%b'


# Hiztory is stored here
HISTFILE=~/.histfile

# The number of commands stored in memory
HISTSIZE=1000

# The number of commands saved in my history file
SAVEHIST=1000

# Append all history to this file
setopt appendhistory
setopt autocd
unsetopt beep nomatch notify
bindkey -v

#######################
# zsh & urxvt options #
#######################

# Run urxvt via my script.
alias urxvt ="/home/nathan/scripts/urxvt.sh"

# less is more.
alias more='less'

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

#############
# Hardware  #
#############

# Brightness control settings.
alias brightup='xbacklight -inc 20'
alias brightdown='xbacklight -dec 20'

# Rotate my wacom tablet.
# Make this automatic in the future!
alias wacom='xsetwacom --set "Wacom Intuos4 6x9 stylus" rotate half && xsetwacom --set "Wacom Intuos4 6x9 cursor" rotate half && xsetwacom --set "Wacom Intuos4 6x9 eraser" rotate half'

# Touchpad on/off.
alias touchon='synclient TouchpadOff=0'
alias touchoff='synclient TouchpadOff=1'

# blanking disable
alias blankoff='xset -dpms; xset s off'

# Suspend my computer.
alias suspend='systemctl suspend'

# Window pager
#alias pager='~/scripts/wmpager.sh && tail -f /tmp/wmpager.sh.pipe | dzen2 -ta l -x 0 -y 0 -w 153 -fn Terminus-8 -bg #002b36 -fg #657b83'
#alias czen='conky -c ~/.conkytoprc | dzen2 -ta l -x 153 -fn Terminus-8 -bg #002b36 -fg #657b83'

# connect with wpa.conf
alias wpa='sudo wpa_supplicant -K -Dwext -i wlan0 -c /run/network//wpa.wlan0/wpa.conf'

# check wpa status
alias wpacli='wpa_cli -p /run/wpa_supplicant -i wlan0 status'

# give skype its own uers account
alias skype='xhost +local: && sudo -u skype /usr/bin/skype'

# start mpd and scmpc
alias pms='pms -p 6606'

# color my searches
alias grep='grep --color=auto'

alias sshorange='~/scripts/sshorange.sh'

# view disowned files in pacman. Dangerous!
alias pacman-disowned='/home/nathan/scripts/pacman-disowned'

# nmap a whole subnet
alias nmaps='sudo nmap -sS -P0'

# start eclim daemon
#alias eclimd = "/usr/share/eclipse/eclimd -Dosgi.instance.area.default=@nathan.home/jeclipse"

# Firefox addon SDK shortcut.
alias addon-sdk="cd /opt/addon-sdk && source bin/activate; cd -"

# Vim for life.
export EDITOR="vim"

# Since there's nothing better out there ...
export BROWSER="firefox"

export GREP_COLOR="1;33"

export LESS="-R"

export CLASSPATH="/usr/share/java/junit.jar"

# scripts in path
export PATH=$PATH:~/scripts:~/.gem/ruby/2.0.0/bin:~/.cabal/bin

export PYTHONPATH=$PYTHONPATH:python/i3-wm

# Some people like $TERM_PROGRAM. Like the solarized colorscheme for vim. I don't.
export TERM_PROGRAM=$TERM

# Steam likes this
export SDL_AUDIODRIVER=alsa

# Eclipse doesn't seem to play nice by default.
export ECLIPSE_HOME="/usr/share/eclipse"

# number of lines kept in history
export HISTSIZE=1000

# number of lines saved in the history after logout
export SAVEHIST=500

# location of history
export HISTFILE=~/.zhistory

#syncad
export SYNCAD_LICENSE_FILE=~/synapticad-17.07d/license.dat

export CCACHE_DIR=~/.ccache

export CHROOT=$HOME/chroot

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
    if [ $1] ; then
        nmap -sS -P0 $1
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

#eclim
alias eclim='/usr/share/eclipse/eclimd -Dnailgun.server.port=9091'
