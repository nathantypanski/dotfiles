{config, pkgs, lib, ... }:

{
  imports = [
    ./zsh.nix
    ./tmux.nix
    ./terminal.nix
  ];
  services.emacs.package = pkgs.emacs-pgtk;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
      https://github.com/nix-community/emacs-overlay/archive/8ae925057bad39c6a72f55dd2ff0b281e2cea714.tar.gz;
    }))
  ];

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
  # programs.home-manager.path = "$HOME/src/github.com/nix-community/home-manager";

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = false;
    enableScDaemon = true;
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 60; command = "${pkgs.swaylock}/bin/swaylock -fF"; }
    ];
    events = [
      { event = "lock"; command = "lock"; }
    ];
  };

  home.keyboard.options = ["ctrl:nocaps"];
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true ;
    config = {
      output = {
        DP-1 = {
          # mode = "5120x1440@99.99Hz";
          mode = "5120x1440@119.974Hz";
        };
      };
      window = {
        border = 3;
      };
      # to list options do `swaymsg -t get_outputs`
      terminal = "alacritty";
      fonts = {
        names = ["pango:Terminus"];
        style = "normal";
        size = 10.0;
      };
      keybindings = let
        modifier = "alt";
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
    ungoogled-chromium
    swaylock
    swayidle
    wl-clipboard
    mako
    # so things like virt-manager don't break
    adwaita-icon-theme 
    alacritty
    wofi
    haskell.compiler.ghc902
    stack
    zathura
    sass
    ispell
    pass
    ydotool
    pavucontrol
    paprefs
    tig
    xfce.thunar
    rustup
    go-tools
    go
    python311
    python311Packages.pip
    python311Packages.virtualenv
    weechat
    # discord
    dig
    imv
    xorg.xhost
    gdb
    cgdb
    qrencode
    monero-cli
    monero-gui
    xmrig
    bore
    ledger
    termbox
    sqlite # required for helm-dash in emacs
    emacs-unstable
    libreoffice
    texlive.combined.scheme-full
    global
    zlib
    libtcod
    python310Packages.venvShellHook
    cmake
    scons
    SDL2
    SDL2.dev
    nix-index
    helix
    wdisplays
    freetype
    wine
    transmission_4-gtk
    weechat
    uqm
    smplayer
  ];

  programs.git = {
    enable = true;
    userName = "ndt";
    userEmail = "ndt@nathantypanski.com";
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
      set number
      set autoindent
      set expandtab

      set shiftwidth=8
      set softtabstop=2
      set textwidth=80
    '';
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
  };

  # xdg-desktop-portal works by exposing a series of D-Bus interfaces
  # known as portals under a well-known name
  # (org.freedesktop.portal.Desktop) and object path
  # (/org/freedesktop/portal/desktop).
  # The portal interfaces include APIs for file access, opening URIs,
  # printing and others.

  manual.manpages.enable = false;
}
