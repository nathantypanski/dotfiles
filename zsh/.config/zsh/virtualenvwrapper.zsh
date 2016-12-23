if [ "$(uname)" = 'Darwin' ]; then
    VIRTUALENVWRAPPER="$HOME"'/Library/Python/2.7/bin/virtualenvwrapper.sh'
    if [[ -f "${VIRTUALENVWRAPPER}" ]]; then
        source "${VIRTUALENVWRAPPER}"
    fi
else
    # virtualenvwrapper in home directory
    VIRTUALENVWRAPPER_LOCAL="$HOME"'/.local/bin/virtualenvwrapper.sh'
    # virtualenvwrapper installed on system
    VIRTUALENVWRAPPER='/usr/bin/virtualenvwrapper.sh'
    if [[ -f "${VIRTUALENVWRAPPER_LOCAL}" ]]; then
        source "${VIRTUALENVWRAPPER_LOCAL}"
    elif [[ -f "${VIRTUALENVWRAPPER}" ]]; then
        source "${VIRTUALENVWRAPPER}"
    fi
    source "$XDG_CONFIG_HOME"'/zsh/keychain.zsh'
fi
