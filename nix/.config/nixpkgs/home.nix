{ config, pkgs, lib, ... }:

{
  imports = [
    ./zsh.nix
    ./tmux.nix
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
  programs.home-manager.path = "$HOME/src/github.com/nix-community/home-manager";

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = false;
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
    wrapperFeatures.gtk = true ; config = {
      terminal = "alacritty";
      systemdIntegration = true;
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
      ## Can't get this to work:
      # input = { "*" = { xkb_options = "caps:ctrl_modifier"; }; };
    };
  };
  home.packages = with pkgs; [
    swaylock
    swayidle
    wl-clipboard
    mako
    alacritty
    wofi
    # haskell.compiler.ghc921
    haskell.compiler.ghc902
    # haskell.compiler.ghc922
    # ghc
    stack
    zathura
    sass
    ispell
    pass
    ydotool
    pavucontrol
    paprefs
    zotero
    xfce.thunar
    rustup
    go-tools
    go
    python39
    python39Packages.pip
    python39Packages.virtualenv
    weechat
    discord
    dig
    imv
    vcv-rack
    xorg.xhost
    gdb
    cgdb
    yubioath-desktop
    arcan.all-wrapped
    # urbit
    qrencode
    monero
    monero-gui
    xmrig
    bore
    ledger
    termbox
    sqlite # required for helm-dash in emacs
    libreoffice
    texlive.combined.scheme-full
    global
    python310Packages.poetry
    zlib
    libtcod
    python310Packages.venvShellHook
    cmake
    scons
    SDL2
    SDL2.dev
    nix-index
    rust-analyzer
    helix
    renoise
    #        > ***
    #  > Unfortunately, we cannot download file jdk-8u281-linux-x64.tar.gz automatically.
    #  > Please go to http://www.oracle.com    echnetwork/java/javase/downloads/jdk8-downloads-2133151.html to download it yourself, and add it to the Nix store
    #  > using either
    #  >   nix-store --add-fixed sha256 jdk-8u281-linux-x64.tar.gz
    #  > or
    #  >   nix-prefetch-url --type sha256 file:///path    o/jdk-8u281-linux-x64.tar.gz
    #  >
    #  > ***
    #  >
    #
    # processing
    freetype
    ungoogled-chromium
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
    extraConfig = ''set number'';
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
  };
}
