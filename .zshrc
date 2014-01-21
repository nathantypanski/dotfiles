# ~/.zshrc
eval $(/usr/bin/keychain -Q -q --nogui --eval --agents ssh,gpg ~/.ssh/id_rsa ~/.ssh/id_rsa_athen@ephesus)

# my stuff
source ~/.zsh/functions.zsh
source ~/.zsh/aliases.zsh
source ~/.zsh/completions.zsh
source ~/.zsh/settings.zsh
source ~/.zsh/prompt.zsh
