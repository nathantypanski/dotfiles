#PROMPT_COMMAND='DEFTITLE="${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}"; echo -ne "\033]0;${TITLE:-$DEFTITLE}\007"'
PS1="\[\033[G\]$PS1"
# Disable Ethernet Wake-On-Lan with the following command:
#  ethtool -s eth0 wol d

# useful shorthand
alias mpd='mpd && mpdas'
alias wacom='xsetwacom --set "Wacom Intuos4 6x9 stylus" rotate half && xsetwacom --set "Wacom Intuos4 6x9 cursor" rotate half && xsetwacom --set "Wacom Intuos4 6x9 eraser" rotate half'
alias aircnu='sudo wpa_supplicant -i wlan0 -c /etc/wpa_supplicant.conf && sleep 10 && sudo dhcpcd wlan0'
alias wpa='sudo wpa_supplicant -K -Dwext -i wlan0 -c /run/network//wpa.wlan0/wpa.conf'
alias pacman-disowned='/home/nathan/scripts/pacman-disowned'
alias netcfg='sudo netcfg -a && sudo NETCFG_DEBUG="yes" netcfg'
alias urxvt='urxvt --meta8'
alias wpacli='wpa_cli -p /run/wpa_supplicant -i wlan0 status'
alias skype='xhost +local: && sudo -u skype /usr/bin/skype'
alias fastreboot='sudo /usr/local/sbin/kexec-reboot.sh 1'
alias suspend='systemctl suspend'

# modified commands
alias grep='grep --color=auto'
alias more='less'

#ls
alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias ll='ls -l --color=auto'
alias lx='ll -BX --color=auto'                   # sort by extension
alias lz='ll -rS --color=auto'                   # sort by size
alias lt='ll -rt --color=auto'                   # sort by date

# safety features
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'                    # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

export EDITOR="vim"
export BROWSER="chromium"
export GREP_COLOR="1;33"
export LESS="-R"

export CLASSPATH="$CLASSPATH:/usr/share/java/junit.jar"

man() {
	env \
		LESS_TERMCAP_mb=$(printf "\e[1;31m") \
		LESS_TERMCAP_md=$(printf "\e[1;31m") \
		LESS_TERMCAP_me=$(printf "\e[0m") \
		LESS_TERMCAP_se=$(printf "\e[0m") \
		LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
		LESS_TERMCAP_ue=$(printf "\e[0m") \
		LESS_TERMCAP_us=$(printf "\e[1;32m") \
			man "$@"
}


## Colorized pacman search
pacs() {
	local CL='\\e['
	local RS='\\e[0;0m'

	echo -e "$(pacman -Ss "$@" | sed "
		/^core/		s,.*,${CL}1;31m&${RS},
		/^extra/	s,.*,${CL}0;32m&${RS},
		/^community/	s,.*,${CL}1;35m&${RS},
		/^[^[:space:]]/	s,.*,${CL}0;36m&${RS},
	")"
}

# startx replacement
stx()
{
    local tty_num=$(tty | grep -oE '[0-9]+$')
    startx -- -logverbose 7 vt$tty_num &>/var/tmp/my_xorg.log
}
