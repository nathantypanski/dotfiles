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
      ls = "ls --color=auto";
      ll = "ls -l";
      la = "ls -a";
    };
    profileExtra = ''
      umask 027
      EDITOR=nvim
      # . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    '';
    sessionVariables = {
      NIXPKGS_ALLOW_UNFREE = "1";
    };
    initContent = ''
      setopt vi
      PROMPT="%F{red}%n%F{white}@%B%F{blue}%m%b %F{yellow}%3~ %F{white}$ "
      bindkey '^R' history-incremental-search-backward
    '';
  };
}
