# irc
alias irc=weechat-curses

alias hc='herbstclient'

# less is more.
alias more='less'

# ls
alias ls='ls --color=auto'

# abbreviated listing
alias la='ls -A'

# sort by extension
alias lx='ll -BX'

# sort by size
alias lz='ll -rS'

# sort by date
alias lt='ll -rt'

# cd
alias ..="cd .."
alias ...="cd ../.."

alias dots='git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'

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

if [ `uname` != 'Darwin' ]; then
    alias ssh='eval $(/usr/bin/keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa ~/.ssh/id_rsa_athen@ephesus) && ssh'
fi

# vim like quit
alias ":q"="exit"

# "newsbeuter" takes too long to type
alias news="newsbeuter"
alias xm='GHC_PACKAGE_PATH="/home/nathan/.xmonad/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d/":"/home/nathan/.ghc/x86_64-linux-7.8.3/package.conf.d/" /home/nathan/.xmonad/.cabal-sandbox/bin/xmonad'

alias x='tmux'

if which ipython &> /dev/null; then
    alias ipy='ipython'
    alias pysh='ipython --profile=pysh'
fi

if which arbtt-stats &> /dev/null; then
    alias arbtt-lately='arbtt-stats --dump-samples'
fi

# Search in a node.js project
alias agnode="ag --js --ignore '*node_modules*' --ignore '*.min.js'"

alias curljson='curl -H "Content-Type: application/json"'
