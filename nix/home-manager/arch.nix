{ config, pkgs, lib, username, homeDirectory, secrets, ... }:

let
  copyCommand = "wl-copy";
  mod = "Mod4";
  envPrintScript = pkgs.writeShellScript "xdg-test" ''
    #!${pkgs.bash}/bin/bash
    printf '(systemd)=(envvar)\n'
    printf '%s=%s\n' XDG_CACHE_HOME "$XDG_CACHE_HOME"
    printf '%s=%s\n' XDG_CONFIG_HOME "$XDG_CONFIG_HOME"
    printf '%s=%s\n' XDG_STATE_HOME "$XDG_STATE_HOME"
    printf '%s=%s\n' XDG_RUNTIME_DIR "$XDG_RUNTIME_DIR"
  '';
  termFont = "Terminus";
in {
  imports = [
    (import ./neovim.nix { inherit config pkgs; })
    (import ./tmux.nix { inherit config pkgs copyCommand; })
    # (import ./river.nix {
    #   inherit config pkgs lib mod termFont homeDirectory;
    # })
    (import ./sway.nix {
      inherit config pkgs lib mod termFont homeDirectory;
      withNixGL = true;
    })
    (import ./zsh.nix { inherit pkgs; })
    (import ./foot.nix { inherit termFont; })
    (import ./newsboat.nix {
      inherit config pkgs;
      browser = lib.getExe config.ndt-home.firefox-jailed;
    })
    (import ./git.nix {
      inherit homeDirectory username;
      userEmail = secrets.userEmail;
    })
    (import ./scripts.nix {
      inherit pkgs lib;
    })
    (import ./firefox.nix {
      inherit config pkgs lib;
    })
    (import ./emacs.nix { inherit config pkgs; })
    (import ./languages.nix { inherit config pkgs; })
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = false;
    enableScDaemon = true;
  };

  home.keyboard.options = ["ctrl:nocaps"];

  home.packages = with pkgs; [
    tmux
    foot
    tailscale

    adwaita-icon-theme
    libglvnd
    wayland-utils
    vulkan-tools
    glxinfo

    floorp
    vivaldi

    wl-clipboard
    mako
    adwaita-icon-theme
    zathura
    ispell
    pass
    tig
    xfce.thunar
    wdisplays
    pavucontrol
    (ungoogled-chromium.override {
      # these args get baked into the wrapper
      commandLineArgs = [
        "--enable-features=UseOzonePlatform"
        "--ozone-platform-hint=auto"
        "--use-gl=egl"
        "--gtk-version=4"
        "--enable-features=WaylandPerSurfaceScale,WaylandUiScale"
      ];
    })

    procps
    fzf
    bashInteractive
    nix-search
    silver-searcher
    aspellDicts.en

    go
    gotools
    # lsps
    jq
    parallel

    nil
    (python313.withPackages (ps: with ps; [
      pip
      virtualenv
    ]))
    poetry

    (pkgs.writeShellScriptBin "rebuild-home" ''
      exec ${homeDirectory}/src/github.com/nathantypanski/dotfiles/nix/arch/rebuild.sh
    '')
    (pkgs.writeShellScriptBin "which-path" ''
      while IFS= read -r line; do
        p="$line/tpm-fido"
         [[ -x "$p" ]] && echo "$p"
      done < <(echo $PATH | tr ':' '\n')
    '')

    age-plugin-yubikey
    age-plugin-tpm
    tpm-fido
    tomb
    passExtensions.pass-tomb
    rage

    # signal rust client
    gurk-rs
    (pkgs.writeShellScriptBin "signal" ''
      ${signal-desktop}/bin/signal-desktop \
          --enable-features=UseOzonePlatform \
          --ozone-platform=wayland $@
    '')

    passage
    yubikey-manager

    transmission_4-gtk
    gitit

    claude-code
    (pkgs.writeShellScriptBin "claude-jailed" ''
      exec firejail --profile=claude-code ${pkgs.claude-code}/bin/claude "$@"
    '')
    xwayland
    xwayland-run
    xorg.xhost
    xorg.xauth

    fontconfig
    font-manager
    ultimate-oldschool-pc-font-pack
    terminus_font
    terminus_font_ttf
    termsyn
    departure-mono
    dina-font

    gbdfed
    fontforge
  ];

  programs.wofi = {
    enable = true;
    settings = {
      font = "Terminus:size=12";
    };
  };


  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    extraFlags = [ "--noask" "--quiet" ];
    keys = [
      "id_ed25519"
    ];
  };

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enableZshIntegration = true;
    extraConfig = ''
      pinentry-program ${pkgs.pinentry-tty}/bin/pinentry-tty
      allow-loopback-pinentry
    '';
  };

  manual.manpages.enable = false;

  home.sessionVariables = {
    XDG_CONFIG_HOME = "${homeDirectory}/.config";
    XDG_CACHE_HOME = "${homeDirectory}/.cache";
    XDG_STATE_HOME = "${homeDirectory}/.local/state";
    PINENTRY_PROGRAM = "${pkgs.pinentry-tty}/bin/pinentry-tty";
    PAGER = "less -R --use-color";
  };

  home.sessionPath = [
    "${pkgs.rage}/bin"
    "${pkgs.age-plugin-yubikey}/bin"
    "${pkgs.age-plugin-tpm}/bin"
  ];

  systemd.user.services.env-print = {
    Unit.Description = "print environment";
    Install.WantedBy = [ "default.target" ];
    Service.Type = "oneshot";
    Service.ExecStart = envPrintScript;
  };
  systemd.user.sessionVariables = {
    XDG_CONFIG_HOME = "${homeDirectory}/.config";
    XDG_CACHE_HOME = "${homeDirectory}/.cache";
    XDG_STATE_HOME = "${homeDirectory}/.local/state";
  };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "zathura.desktop";
      "application/x-extension-htm" = "firefox.desktop";
      "application/x-extension-html" = "firefox.desktop";
      "application/x-extension-shtml" = "firefox.desktop";
      "application/x-extension-xht" = "firefox.desktop";
      "application/x-extension-xhtml" = "firefox.desktop";
      "application/xhtml+xml" = "firefox.desktop";
      "text/html" = "firefox.desktop";
      "x-scheme-handler/chrome" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      # "x-scheme-handler/magnet" = "userapp-transmission-gtk-CUDW72.desktop";
      "x-scheme-handler/sgnl" = "signal.desktop";
      "x-scheme-handler/signalcaptcha" = "signal.desktop";
      "x-scheme-handler/magnet" = "transmission-gtk.desktop";
    };
  };


  fonts.fontconfig = {
    enable = true;
  };

  systemd.user.enable = true;

  home.file.".config/firejail/claude-code.profile".source = "${homeDirectory}/src/github.com/nathantypanski/dotfiles/nix/home-manager/files/claude-code.profile";

}
