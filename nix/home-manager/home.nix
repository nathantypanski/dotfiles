{ config, pkgs, lib, username, homeDirectory, secrets, termFont, ... }:

{
  imports = [
     ./zsh.nix
    (import ./tmux.nix ({
      inherit config pkgs;
      copyCommand = "wl-copy";
    }))
    (import ./foot.nix ({
      inherit config pkgs termFont;
    }))
  ];

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 300; command = "${pkgs.swaylock}/bin/swaylock -fF"; }
    ];
    events = [
      { event = "lock"; command = "lock"; }
    ];
  };
  home.keyboard.options = ["ctrl:nocaps"];

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true ;
    extraConfig = ''
      output DP-2 mode 5120x1440@99.99Hz
    '';
    config = {
      # output = {
      #   # TODO: why doesn't this work?
      #   DP-2 = {
      #     mode = "5120x1440@99Hz";
      #   };
      # };
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
      ## Can't get this to work:
      # input = { "*" = { xkb_options = "caps:ctrl_modifier"; }; };
    };
  };

  home.packages = with pkgs; [
    emacs-unstable

    foot
    swaylock
    swayidle
    wl-clipboard
    mako
    # so things like virt-manager don't break
    adwaita-icon-theme 
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
    # zotero # Package ‘zotero-6.0.26’ in
    # /nix/store/cyz72bh2sp5jk0asal64cpm09vrggb97-nixos-23.05.4738.41de143fda10/nixos/pkgs/applications/office/zotero/default.nix:151
    # is marked as insecure, refusing to evaluate.
    tig
    xfce.thunar
    rustup
    go-tools
    go
    weechat
    discord
    dig
    imv
    vcv-rack
    xorg.xhost
    gdb
    cgdb
    # urbit
    qrencode
    monero-cli
    monero-gui
    xmrig
    bore
    ledger
    termbox
    sqlite # required for helm-dash in emacs
    libreoffice
    texlive.combined.scheme-medium
    global
    zlib
    libtcod
    python311Packages.venvShellHook
    python311
    python311Packages.pip
    python311Packages.virtualenv
    cmake
    scons
    SDL2
    SDL2.dev
    nix-index
    helix
    renoise
    wdisplays
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
    wine
    # TODO: update to t4
    transmission_3-gtk
    # ipfs
  ];

  programs.git = {
    enable = true;
    userName = username;
    userEmail = secrets.userEmail;
    # home-manager only supports gnupg, so try to workaround
    signing = {
      signByDefault = true;
    };
    aliases = {
      co = "checkout";
      br = "branch";
      ci = "commit";
      S  = "status";
      s  = "status --short";
    };
    extraConfig = {
      gpg.format = "ssh";
      gpg."ssh".allowedSignersFile = "${homeDirectory}/.config/git/allowed_signers";
      user.signingkey = "${homeDirectory}/.ssh/id_ed25519.pub";
    };
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimdiffAlias = true;
    extraConfig = ''set number'';
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = false;
    enableScDaemon = true;
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
  };

  manual.manpages.enable = false;
}
