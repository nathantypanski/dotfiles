{ pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;
  grmlZshrc = if isDarwin
    then "${pkgs.grml-zsh-config}/etc/zsh/zshrc"
              else "/etc/zsh/zshrc";
  brewShell = "/opt/homebrew/bin/brew";
in
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
      g = "git";
      ls = "ls --color=auto";
      ll = "ls -l";
      la = "ls -a";
      userctl = "systemctl --user";
    };
    profileExtra = ''
      umask 027
      EDITOR=nvim
    '';
    sessionVariables = {
      NIXPKGS_ALLOW_UNFREE = "1";
    };
    envExtra = ''
      GOROOT="$HOME"
    '';
    initContent = ''
      PROMPT="%F{red}%n%F{white}@%B%F{blue}%m%b %F{white}[%F{yellow}%5~%F{white}]%F %F{white}$ "

      # I'm currently relying on system-provided grml prompts.
      #
      # PROMPT="%F{red}%n%F{white}@%B%F{blue}%m%b %F{white}[%F{yellow}%5~%F{white}]%F %F{white}''$ "

      # source grml zshrc
      if [[ -f "''${grmlZshrc}" ]]; then
        source "''${grmlZshrc}"
      fi

      # TODO: remove this from nix if running on Linux
      if [[ -f "''${brewShell}" ]]; then
            eval "$(''${brewShell} shellenv)"
      fi

      setopt vi
      bindkey '^R' history-incremental-search-backward

      function gptemacs() {
        if ! [[ -n "''${OPENAI_API_KEY}" && -n "''${ANTHROPIC_API_KEY}" ]]; then
          echo >&2 "setting OPENAI_API_KEY and ANTHROPIC_API_KEY using passage"
          export OPENAI_API_KEY="$(passage openai-api-key-redbow)"
          export ANTHROPIC_API_KEY="$(passage console.anthropic.com/api-redbow)"
        else
          echo >&2 "OPENAI_API_KEY, ANTHROPIC_API_KEY already set"
        fi
        echo >&2 "launching emacs ..."
        emacs -nw "$@"
      }
    '';
  };
}
