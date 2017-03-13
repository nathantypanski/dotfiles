# ~/.zshrc

source "${XDG_CONFIG_HOME}/zsh/functions.zsh"
source "${XDG_CONFIG_HOME}/zsh/aliases.zsh"
source "${XDG_CONFIG_HOME}/zsh/completions.zsh"
source "${XDG_CONFIG_HOME}/zsh/settings.zsh"
source "${XDG_CONFIG_HOME}/zsh/ruby.zsh"
source "${XDG_CONFIG_HOME}/zsh/rust.zsh"
source "${XDG_CONFIG_HOME}/zsh/prompt.zsh"
if [ "$(uname)" != 'Darwin' ]; then
    source "${XDG_CONFIG_HOME}"'/zsh/keychain.zsh'
fi

if [ -f "$XDG_CONFIG_HOME/zsh/mongodb.zsh" ]; then
    source "$XDG_CONFIG_HOME/zsh/mongodb.zsh"
fi
