# Append all history to this file
setopt appendhistory
setopt autocd
unsetopt beep nomatch notify
bindkey -v

bindkey '^R' history-incremental-search-backward
zle -N ranger-cd
bindkey '^O' ranger-cd

# In grml's zshrc, this enables vi mode instead of emacs by default.
setopt vi
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


eval $(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa ~/.ssh/id_rsa_athen@ephesus)
