# Title the terminal with the current directory, if the terminal is
# titleable ($IS_TITLEABLE_TERM is set).
title-current-dir() {
    if [[ $IS_TITLEABLE_TERM -eq 1 ]]; then
        print -Pn "\e]0;%m:%~\a"
    fi
}

# Title the terminal with the running program, if the terminal is
# titleable ($IS_TITLEABLE_TERM is set).
title-current-prog() {
    if [[ $IS_TITLEABLE_TERM -eq 1 ]]; then
        print -Pn "\e]0;$1\a"
    fi
}

# Determine whether this terminal is titleable, and set the appropriate
# environment variables if it is.
config-titleable () {
    case $TERM in
            *rxvt*|*term)
                IS_TITLEABLE_TERM=1
            ;;
    esac
}

check-set-titles () {
    if [[ $IS_TITLEABLE_TERM -eq 1 ]]; then
        add-zsh-hook precmd title-current-dir
        add-zsh-hook preexec title-current-prog
    fi
}

function zle-keymap-select {
  zle reset-prompt

  if [[ $KEYMAP = "vicmd" ]]; then
    echo -ne "\033]12;10\007"
  else
    echo -ne "\033]12;6\007"
  fi
}

function zle-line-finish {
  echo -ne "\033]12;6\007"
}

zle -N zle-keymap-select
zle -N zle-line-finish

# Append all history to history file
setopt appendhistory
# share history between terminals
setopt inc_append_history
# cd when I just type a directory name
setopt autocd
unsetopt beep nomatch notify
bindkey -v

#use extended regex
setopt re_match_pcre

# better globbing, e.g. `rm *~*.tex` to remove all non-.tex files in a dir
setopt extendedglob

# Let me add zsh hooks with a function call
autoload -Uz add-zsh-hook

bindkey '^R' history-incremental-search-backward
bindkey '^H' back
zle -N ranger-cd
bindkey '^O' ranger-cd

# Open urxvt here
zle -N open-urxvt-here
bindkey '^J' open-urxvt-here

setopt vi

#eval $(keychain --eval --agents ssh -Q --quiet $(cat ~/.keychain/keyfiles))

for sd_cmd in systemctl systemd-analyze systemd-run; do
    alias $sd_cmd='DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/dbus/user_bus_socket" '$sd_cmd
done

if [ $EUID -ne 0 ] ; then
    envfile="$HOME/.gpg-agent-info"
    if [[ -e "$envfile" ]] && kill -0 $(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2) 2>/dev/null; then
        eval "$(cat '$envfile')"
        export GPG_AGENT_INFO  # the env file does not contain the export statement
    fi
fi

config-titleable
check-set-titles
