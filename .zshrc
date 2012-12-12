###############
# zsh options #
###############
zstyle ':completion:*' menu select

#########
# alias #
#########

# rotate tablet for lefties
alias wacom='xsetwacom --set "Wacom Intuos4 6x9 stylus" rotate half && xsetwacom --set "Wacom Intuos4 6x9 cursor" rotate half && xsetwacom --set "Wacom Intuos4 6x9 eraser" rotate half'

# connect with wpa.conf
alias wpa='sudo wpa_supplicant -K -Dwext -i wlan0 -c /run/network//wpa.wlan0/wpa.conf'

# touchpad onoff
alias touchon='synclient TouchpadOff=0'
alias touchoff='synclient TouchpadOff=1'

alias urxvt='urxvt --meta8'
# check wpa status
alias wpacli='wpa_cli -p /run/wpa_supplicant -i wlan0 status'

alias skype='xhost +local: && sudo -u skype /usr/bin/skype'

alias suspend='systemctl suspend'
# color my searches
alias grep='grep --color=auto'

alias more='less'

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

# view disowned files in pacman. Dangerous!
alias pacman-disowned='/home/nathan/scripts/pacman-disowned'
# start eclim daemon
#alias eclimd = "/usr/share/eclipse/eclimd -Dosgi.instance.area.default=@nathan.home/jeclipse"

# envars
export EDITOR="urxvt -e vim"
export BROWSER="firefox"
export GREP_COLOR="1;33"
export LESS="-R"
export CLASSPATH="/usr/share/java/junit.jar"
#steam
export SDL_AUDIODRIVER=alsa
export ECLIPSE_HOME="/usr/share/eclipse"

# number of lines kept in history
export HISTSIZE=1000
# number of lines saved in the history after logout
export SAVEHIST=1000
# location of history
export HISTFILE=~/.zhistory
# append command to history file once executed
setopt inc_append_history
autoload -U compinit
compinit
#enable set/save zsh theme
autoload -Uz promptinit
promptinit
prompt walters
# directory in title bar
chpwd() {
  [[ -o interactive ]] || return
  case $TERM in
    sun-cmd) print -Pn "\e]l%~\e\\"
      ;;
    *xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;%~\a"
      ;;
  esac
}
mktar() { tar cvf "${1%%/}.tar" "${1%%/}/"; }
mktgz() { tar cvzf "${1%%/}.tar.gz" "${1%%/}/"; }
mktbz() { tar cvjf "${1%%/}.tar.bz2" "${1%%/}/"; }

# set window icon
xseticon -id $WINDOWID /usr/share/icons/Faenza/apps/32/terminal.png

#eclim
alias eclim='/usr/share/eclipse/eclimd -Dnailgun.server.port=9091'
