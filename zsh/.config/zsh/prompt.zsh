# fancy zsh colors in prompt
autoload -U colors && colors

#PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg_no_bold[yellow]%}%1~ %{$reset_color%}$ "
PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg_no_bold[yellow]%}%1~ %{$reset_color%}$ "
RPROMPT="%{$reset_color%}[%(?.%{$fg_bold[green]%}.%{$fg_bold[red]%})%?%{$reset_color%}]"
