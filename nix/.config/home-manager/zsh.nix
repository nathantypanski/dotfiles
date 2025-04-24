{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    syntaxHighlighting = {
      enable = true;
    };
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
      # . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    '';
    sessionVariables = {
      NIXPKGS_ALLOW_UNFREE = "1";
    };
    initContent = ''
      PROMPT="%F{red}%n%F{white}@%B%F{blue}%m%b %F{yellow}%3~ %F{white}$ "
      bindkey '^R' history-incremental-search-backward
      alias ls='ls --color=auto'
      alias ll='ls -l'
      alias ll='ls -a'
    '';
  };
}
