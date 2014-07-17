# irc
alias irc=weechat-curses

# Run urxvt via my script.
alias urxvt="/home/nathan/bin/urxvt.sh"

alias hc='herbstclient'

# less is more.
alias more='less'

# ls
alias ls='ls --color=auto'

# abbreviated listing
alias la='ls -a --color=auto'

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

# systemd users
alias userctl="systemctl --user"

# tmux
alias xls="tmux list-sessions"
alias xat="tmux attach -t"
alias x="tmux"

# vim is v
alias v="vim"
alias e="~/bin/ec"

alias ssh='eval $(/usr/bin/keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa ~/.ssh/id_rsa_athen@ephesus) && ssh'

# vim like quit
alias ":q"="exit"

# "newsbeuter" takes too long to type
alias news="newsbeuter"
