{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
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
    '';
    sessionVariables = {
      NIXPKGS_ALLOW_UNFREE = "1";
    };
    envExtra = ''
      GOROOT=$HOME
    '';
    initContent = ''
      setopt vi
      PROMPT="%F{red}%n%F{white}@%B%F{blue}%m%b %F{white}[%F{yellow}%5~%F{white}]%F %F{white}$ "
      bindkey '^R' history-incremental-search-backward
    '';
  };
}
