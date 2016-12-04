# ~/.zshrc

if [ "$(uname)" = 'Darwin' ]; then
    VIRTUALENVWRAPPER="$HOME"'/Library/Python/2.7/bin/virtualenvwrapper.sh'
    if [[ -f "${VIRTUALENVWRAPPER}" ]]; then
        source "${VIRTUALENVWRAPPER}"
    fi
else
    VIRTUALENVWRAPPER="$HOME"'/.local/bin/virtualenvwrapper.sh'
    if [[ -f "${VIRTUALENVWRAPPER}" ]]; then
        source "${VIRTUALENVWRAPPER}"
    fi
    source "$XDG_CONFIG_HOME"'/zsh/keychain.zsh'
fi

source "$XDG_CONFIG_HOME"'/zsh/functions.zsh'
source "$XDG_CONFIG_HOME"'/zsh/aliases.zsh'
source "$XDG_CONFIG_HOME"'/zsh/completions.zsh'
source "$XDG_CONFIG_HOME"'/zsh/settings.zsh'
source "$XDG_CONFIG_HOME"'/zsh/ruby.zsh'
source "$XDG_CONFIG_HOME"'/zsh/rust.zsh'
source "$XDG_CONFIG_HOME"'/zsh/prompt.zsh'
