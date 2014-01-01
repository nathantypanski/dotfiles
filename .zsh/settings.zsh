# Append all history to this file
setopt appendhistory
setopt autocd
unsetopt beep nomatch notify
bindkey -v

bindkey '^R' history-incremental-search-backward
bindkey '^H' back
zle -N ranger-cd
bindkey '^O' ranger-cd

setopt vi

eval $(keychain --eval --agents ssh -Q --quiet $(cat ~/.keychain/keyfiles))

for sd_cmd in systemctl systemd-analyze systemd-run; do
    alias $sd_cmd='DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/dbus/user_bus_socket" '$sd_cmd
done

if [ $EUID -ne 0 ] ; then
    envfile="$HOME/.gpg-agent-info"
    if [[ -e "$envfile" ]] && kill -0 $(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2) 2>/dev/null; then
        eval "$(cat "$envfile")"
        export GPG_AGENT_INFO  # the env file does not contain the export statement
    fi
fi
