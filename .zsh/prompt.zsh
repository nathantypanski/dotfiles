# fancy zsh colors in prompt
autoload -U colors && colors 

# If the PROMPT_SUBST option is set, the prompt string is first subjected to
# parameter expansion, command substitution and arithmetic expansion.
setopt PROMPT_SUBST

# # export PROMPT="%1~%(λ#.#.$BLUE$)$NO_COLOR "
source ~/.zsh/git-prompt.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main pattern)
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
echo -ne "\033]12;6\007"
PROMPT='%~/ %{$reset_color%}%{$terminfo[bold]$fg[blue]%}λ %{$reset_color%}'
if [[ "$(whoami)" == "root" ]]; then
    PROMPT='%~/ %{$reset_color%}%{$terminfo[bold]$fg[red]%}λ %{$reset_color%}'
fi
export PROMPT
