# home-manager/darwin.nix

{ config, pkgs, username, homeDirectory, secrets, ... }:

let
  copyCommand = "pbcopy";
in {
  imports = [
    ./zsh.nix
    (import ./tmux.nix { inherit config pkgs copyCommand; })
    (import ./neovim.nix { inherit config pkgs; })
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
  home.packages = [
    pkgs.tmux
    pkgs.emacs-git
  ];

  # Let Home Manager install and manage itself.
  # programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = username;
    userEmail = secrets.userEmail;
    # home-manager only supports gnupg, so try to workaround
    signing = {
      signByDefault = true;
    };
    extraConfig = {
      gpg.format = "ssh";
      gpg."ssh".allowedSignersFile = "/Users/ndt/.git_allowed_signers";
      user.signingkey = "/Users/ndt/.ssh/id_ed25519.pub";
    };
  };
}
