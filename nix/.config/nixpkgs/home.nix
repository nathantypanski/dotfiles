{ config, pkgs, lib, ... }:

{
  imports = [ ./zsh.nix ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ndt";
  home.homeDirectory = "/home/ndt";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = false;
  };

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true ;
    config = {
      terminal = "alacritty";
      fonts = {
        names = ["pango:Terminus"];
        style = "normal";
        size = 10.0;
      };
      keybindings = let
        modifier = config.wayland.windowManager.sway.config.modifier;
        terminal = config.wayland.windowManager.sway.config.terminal;
      in lib.mkOptionDefault {
        "${modifier}+Shift+Return" = "exec ${terminal}";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";
        "${modifier}+Shift+apostrophe" = "split v";
        "${modifier}+apostrophe" = "split h";
        "${modifier}+f" = "fullscreen";
        "${modifier}+s" = "stacking";
        "${modifier}+w" = "tabbed";
        "${modifier}+e" = "toggle split";
        "${modifier}+space" = "focus mode_toggle";
        "${modifier}+Shift+space" = "floating toggle";
        "${modifier}+bracketleft" = "focus parent";
        "${modifier}+Ctrl+r" = "reload";
        # move focused container to workspace
        "${modifier}+Shift+1" = "move container to workspace 1";
        "${modifier}+Shift+2" = "move container to workspace 2";
        "${modifier}+Shift+3" = "move container to workspace 3";
        "${modifier}+Shift+4" = "move container to workspace 4";
        "${modifier}+Shift+5" = "move container to workspace 5";
        "${modifier}+Shift+6" = "move container to workspace 6";
        "${modifier}+Shift+7" = "move container to workspace 7";
        "${modifier}+Shift+8" = "move container to workspace 8";
        "${modifier}+Shift+9" = "move container to workspace 9";
        "${modifier}+Shift+0" = "move container to workspace 10";
      };
    };
  };
  home.packages = with pkgs; [
    swaylock
    swayidle
    wl-clipboard
    mako
    alacritty
    wofi
    haskell.compiler.ghc921
    stack
    zathura
    sass
    ispell
    pass
  ];

  programs.git = {
    enable = true;
    userName = "ndt";
    userEmail = "ndt@nathantypanski.com";
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
  };
}
