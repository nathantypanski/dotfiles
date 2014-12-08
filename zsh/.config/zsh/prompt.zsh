# fancy zsh colors in prompt
autoload -U colors && colors

# If the PROMPT_SUBST option is set, the prompt string is first subjected to
# parameter expansion, command substitution and arithmetic expansion.
setopt PROMPT_SUBST

# Indicate what vi mode is active
# Thanks <https://github.com/everett1992/dotfiles>
vim_ins_mode="%{$fg[blue]%}"
vim_nml_mode="%{$fg[green]%}"
vim_mode=$vim_ins_mode

function zle-keymap-select {
    vim_mode="${${KEYMAP/vicmd/${vim_nml_mode}}/(main|viins)/${vim_ins_mode}}"
    zle reset-prompt
}

zle -N zle-keymap-select

function zle-line-finish {
    vim_mode=$vim_ins_mode
}
zle -N zle-line-finish


source "$XDG_CONFIG_HOME"'/zsh/git-prompt.zsh'

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main pattern)

echo -ne "\033]12;6\007"

PROMPT='%~/ %{$reset_color%}%{$terminfo[bold]%}${vim_mode}» %{$reset_color%}'
if [[ "$(whoami)" == "root" ]]; then
    PROMPT='%~/ %{$reset_color%}%{$terminfo[bold]%}${vim_mode}# %{$reset_color%}'
fi

case $TERM in
    'termite'|\
    '*xterm*'|\
    'rxvt'|\
    'rxvt-unicode'|\
    'rxvt-256color'|\
    'rxvt-unicode-256color'|\
    '(dt|k|E)term')

        precmd () {
            print -Pn "\e]0;[%n@%M][%~]%#\a"
        }

        preexec () {
            print -Pn "\e]0;[%n@%M][%~]%# ($1)\a"
        }

    ;;
    'screen'|'screen-256color')

        precmd () {
            print -Pn "\e]83;title \"$1\"\a"
            print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~]\a"
        }

        preexec () {
            print -Pn "\e]83;title \"$1\"\a"
            print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~] ($1)\a"
        }

    ;;
esac

export PROMPT
