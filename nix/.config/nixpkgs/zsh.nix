{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    history = {
      save = 10000;
      size = 10000;
      share = true;
      extended = true;
      ignoreSpace = true;
      ignorePatterns = [
        "rm *"
        "pkill *"
      ];
    };
    shellAliases = {
      x = "tmux";
    };
    profileExtra = ''
      umask 027
    '';
    sessionVariables = {
      PROMPT = "%F{red}%n%F{white}@%B%F{blue}%m%b %F{yellow}%3~ %F{white}$ ";
      RPROMPT = "%F{white}[%(?.%F{green}.%F{red})%?%F{white}]";
      NIXPKGS_ALLOW_UNFREE = "1";
    };
    initExtra = ''
      bindkey '^R' history-incremental-search-backward
      alias ls='ls --color=auto'
      alias ll='ls -l'
      alias ll='ls -a'
    '';
  };
}
