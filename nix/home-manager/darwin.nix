# home-manager/darwin.nix

{ config, pkgs, username, homeDirectory, secrets, ... }:

let
  copyCommand = "pbcopy";
in {
  imports = [
    (import ./zsh.nix {
      inherit pkgs;
      enableCompletion = true;
    })
    ./scripts.nix
    (import ./tmux.nix { inherit config pkgs copyCommand; })
    (import ./neovim.nix { inherit config pkgs; })
    (import ./emacs.nix { inherit config pkgs; })
    (import ./git.nix {
      inherit homeDirectory username secrets;
      userEmail = secrets.userEmail;
    })
  ];

  home.username = username;
  home.homeDirectory = homeDirectory;
  # Home Manager needs a bit of information about you and the paths it should
  # manage.

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    docker
    podman
    tmux
    nix-search
    silver-searcher
    tig
    utm
    transmission_4-qt5
    claude-code
    ripgrep
    niv
    tree-sitter-grammars.tree-sitter-typescript
    javascript-typescript-langserver
    electrum
    electrum-ltc
    solana-cli
    go-ethereum
    yubikey-manager
    uefi-firmware-parser
    uefi-run
    uefitool
    nix-search
    utm
    go
    bazel
    beancount_2
    beancount-language-server
    fava
    ledger2beancount
    xan
    pass
    claude-code
    (aspellWithDicts
      (dicts: with dicts; [ de en en-computers en-science es fr la ]))
    awk-language-server
    pentestgpt
    kanha
    python312Packages.sectools

    # OpenSSH with FIDO2/U2F support for YubiKey security keys
    (openssh.override {
      withFIDO = true;
    })
    keychain  # ssh-agent manager for better YubiKey support
    git
  ];

  home.sessionVariables = {
    # Note: PATH modifications don't work reliably here on macOS
    # Use profileExtra in zsh.nix instead for PATH changes
  };


  programs.rbenv = {
    enableZshIntegration = true;
    enable = true;
  };

  # Let Home Manager install and manage itself
  programs.home-manager.enable = true;
}
