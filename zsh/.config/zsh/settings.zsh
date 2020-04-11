# Title the terminal with the current directory, if the terminal is
# titleable ($IS_TITLEABLE_TERM is set).
title-current-dir() {
    if [[ $IS_TITLEABLE_TERM -eq 1 ]]; then
        print -Pn "\e]0;%m:%~\a"
    fi
}

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


# Append all history to history file
setopt appendhistory
# share history between terminals
# setopt inc_append_history

# automatically pushd when cd'ing
setopt auto_pushd

unsetopt beep nomatch notify
bindkey -v

# Don't try to correct me.
unsetopt correct

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

zle -N open-terminal-here
bindkey '^J' open-terminal-here

setopt vi
setopt transient_rprompt

if [ $EUID -ne 0 ] ; then
    envfile="$HOME/.gpg-agent-info"
    if [[ -e "$envfile" ]] && kill -0 $(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2) 2>/dev/null; then
        export $(cat $envfile)
    fi
fi
