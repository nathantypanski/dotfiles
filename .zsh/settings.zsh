# Append all history to this file
setopt appendhistory
setopt autocd
unsetopt beep nomatch notify
bindkey -v

bindkey '^R' history-incremental-search-backward
bindkey '^H' back
zle -N ranger-cd
bindkey '^O' ranger-cd

export PROMPT="%1~%(#.#.$BLUE$)$NO_COLOR "

setopt vi

eval $(keychain --eval --agents ssh -Q --quiet $(cat ~/.keychain/keyfiles))
