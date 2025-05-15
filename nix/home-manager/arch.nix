{ config, pkgs, lib, username, homeDirectory, secrets, ... }:

let
  copyCommand = "wl-copy";
  mod = "Mod4";
  envPrintScript = pkgs.writeShellScript "test" ''
    #!${pkgs.bash}/bin/bash
    printf '(systemd)=(envvar)\n'
    printf '%s=%s\n' XDG_CACHE_HOME "$XDG_CACHE_HOME"
    printf '%s=%s\n' XDG_CONFIG_HOME "$XDG_CONFIG_HOME"
    printf '%s=%s\n' XDG_STATE_HOME "$XDG_STATE_HOME"
    printf '%s=%s\n' XDG_RUNTIME_DIR "$XDG_RUNTIME_DIR"
  '';
in {
  imports = [
    (import ./neovim.nix { inherit config pkgs; })
    (import ./tmux.nix { inherit config pkgs copyCommand; })
    (import ./sway.nix { inherit config pkgs lib mod; })
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
    mesa
    wayland
    waybar
    brightnessctl
    tailscale
    i3status
    terminus_font
    libglvnd
    xwayland
    swayidle
    wl-clipboard
    mako
    adwaita-icon-theme
    wofi
    zathura
    ispell
    pass
    tig
    xfce.thunar
    wdisplays
    wine
    firefox-devedition

    (ungoogled-chromium.override {
      # these args get baked into the wrapper
      commandLineArgs = [
        "--enable-features=UseOzonePlatform"
        "--ozone-platform-hint=auto"
        "--use-gl=egl"
        "--force-device-scale-factor=0.6"
        "--gtk-version=4"
        "--enable-features=WaylandPerSurfaceScale,WaylandUiScale"
      ];
    })

    dconf-editor
    sway-contrib.grimshot
    swaybg
    fzf
    bashInteractive
    brightnessctl
    nix-search
    terminus_font_ttf
    emacs-unstable
    silver-searcher
    aspellDicts.en
    pavucontrol

    go
    gotools
    # lsps
    gopls
    rust-analyzer
    nil
    bash-language-server
    python313
    python313Packages.pip
    python313Packages.virtualEnv
    poetry

    (pkgs.writeShellScriptBin "pick-foot" ''
      exec ${pkgs.foot}/bin/foot --app-id=launcher --title=launcher -e 'bash' '-c' 'compgen -c | grep -v fzf | sort -u | fzf --layout=reverse | xargs -r swaymsg -t command exec'
    '')
    (pkgs.writeShellScriptBin "rebuild-home" ''
      exec /home/ndt/src/github.com/nathantypanski/dotfiles/nix/arch/rebuild.sh
    '')

    age-plugin-yubikey
    age-plugin-tpm
    tomb
    passExtensions.pass-tomb
    (pkgs.runCommand "age-wrapper" { buildInputs = [ pkgs.makeWrapper ]; } ''
        mkdir -p $out/bin
        makeWrapper ${pkgs.rage}/bin/rage $out/bin/rage \
        --set PATH ${pkgs.age-plugin-yubikey}/bin \
        --set PATH ${pkgs.age-plugin-tpm}/bin \
        --set PINENTRY_PROGRAM ${pkgs.pinentry-curses}/bin/pinentry-curses
    '')
    passage
    yubikey-manager

    firejail
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
    '';
  };

  manual.manpages.enable = false;

  home.sessionVariables = {
    XDG_CONFIG_HOME = "${homeDirectory}/.config";
    XDG_CACHE_HOME = "${homeDirectory}/.cache";
    XDG_STATE_HOME = "${homeDirectory}/.local/state";
  };

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

}
