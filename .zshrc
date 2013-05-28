#######################
# zsh & urxvt options #
#######################

# Give me auto completion.
zstyle ':completion:*' menu select

# Append command to history file once executed.
setopt inc_append_history
autoload -U compinit
compinit

# # Set/save zsh theme.
autoload -Uz promptinit
promptinit
prompt walters

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

#ls
alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias ll='ls -l --color=auto'
alias lx='ll -BX --color=auto'                   # sort by extension
alias lz='ll -rS --color=auto'                   # sort by size
alias lt='ll -rt --color=auto'                   # sort by date

#cd
alias ..="cd .."
alias ...="cd ../.."

# git
alias gitps="git push origin master"
alias gitp="git pull"
alias gitc="git commit -a"
alias gitm="git commit "
alias giti="git init"
alias dotfiles='git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'

# safety features
#alias cp='cp -i'
#alias mv='mv -i'
#alias rm='rm -i'                    # 'rm -i' prompts for every file
#alias ln='ln -i'
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

# Suspend my computer.
alias suspend='systemctl suspend'

# Window pager
alias pager='~/scripts/wmpager.sh && tail -f /tmp/wmpager.sh.pipe | dzen2 -ta l -x 0 -y 0 -w 153 -fn Terminus-8 -bg #002b36 -fg #657b83'
alias czen='conky -c ~/.conkytoprc | dzen2 -ta l -x 153 -fn Terminus-8 -bg #002b36 -fg #657b83'

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
export PATH=$PATH:~/scripts

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

# ~/.ccache to ramdisk
export CCACHE_DIR=/mnt/ramdisk/ccache

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
