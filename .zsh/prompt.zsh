# fancy zsh colors in prompt
autoload -U colors && colors 

# If the PROMPT_SUBST option is set, the prompt string is first subjected to
# parameter expansion, command substitution and arithmetic expansion.
setopt PROMPT_SUBST

export PROMPT="%1~%(#.#.$BLUE$)$NO_COLOR "
source ~/.zsh/git-prompt.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main pattern)
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
