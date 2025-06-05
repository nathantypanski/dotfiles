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
  termFont = "Terminus";
  firefox-jailed = (pkgs.writeShellScriptBin "firefox-jailed" ''
      # this allows firefox to work under nixGL sway
      MESA_DRI_PATH=$(echo "''$LIBGL_DRIVERS_PATH" | cut -d':' -f1)
      if [ -n "''$MESA_DRI_PATH" ]; then
        # Convert /path/to/mesa/lib/dri to /path/to/mesa/lib/gbm
        export GBM_BACKENDS_PATH="''${MESA_DRI_PATH%/dri}/gbm"
        echo "Setting GBM_BACKENDS_PATH: ''$GBM_BACKENDS_PATH"
      fi

      export MOZ_ENABLE_WAYLAND=1
      exec firejail ${lib.getExe pkgs.firefox-devedition} "$@"
    '');
in {
  imports = [
    (import ./neovim.nix { inherit config pkgs; })
    (import ./tmux.nix { inherit config pkgs copyCommand; })
    (import ./sway.nix {
      inherit config pkgs lib mod termFont homeDirectory;
      withNixGL = true;
    })
    (import ./zsh.nix { inherit pkgs; })
    (import ./foot.nix { inherit termFont; })
    (import ./newsboat.nix {
      inherit config pkgs;
      browser = lib.getExe firefox-jailed;
    })
    (import ./git.nix {
      inherit homeDirectory username;
      userEmail = secrets.userEmail;
    })
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
    mesa
    wayland
    brightnessctl
    tailscale

    nixgl.nixGLMesa

    fontconfig
    font-manager
    adwaita-icon-theme
    terminus_font
    terminus_font_ttf
    libglvnd
    xwayland
    swayidle

    wl-clipboard
    mako
    adwaita-icon-theme
    zathura
    ispell
    pass
    tig
    xfce.thunar
    wdisplays
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

    dconf-editor
    sway-contrib.grimshot
    imv
    swaybg
    procps
    fzf
    bashInteractive
    brightnessctl
    nix-search
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
    python313Packages.virtualenv
    python313Packages.python-lsp-server
    python313Packages.pylsp-mypy
    python313Packages.pylsp-rope
    python313Packages.python-lsp-ruff
    poetry

    #mcp
    python313Packages.mcp

    (pkgs.writeShellScriptBin "pick-foot" ''
      exec ${pkgs.foot}/bin/foot --app-id=launcher --title=launcher -e 'bash' '-c' 'compgen -c | grep -v fzf | sort -u | fzf --layout=reverse | xargs -r swaymsg -t command exec'
    '')
    (pkgs.writeShellScriptBin "rebuild-home" ''
      exec /home/ndt/src/github.com/nathantypanski/dotfiles/nix/arch/rebuild.sh
    '')
    firefox-devedition
    firefox-jailed
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
    pinentry-emacs
    rage
    (pkgs.writeShellScriptBin "rage-emacs" ''
      #!/usr/bin/env bash
      export PINENTRY_PROGRAM=${pkgs.pinentry-emacs}/bin/pinentry-emacs
      export PATH=${pkgs.age-plugin-yubikey}/bin:${pkgs.age-plugin-tpm}/bin:${pkgs.pinentry-emacs}/bin}:$PATH
      exec ${pkgs.rage}/bin/rage "$@"
    '')
    (pkgs.writeShellScriptBin "signal" ''
      ${signal-desktop}/bin/signal-desktop \
          --enable-features=UseOzonePlatform \
          --ozone-platform=wayland $@
    '')
    signal-desktop
    swayimg
    passage
    yubikey-manager
  ];

  programs.wofi = {
    enable = true;
    settings = {
      font = "Terminus:size=12";
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-git-pgtk;
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
      allow-loopback-pinentry
      allow-emacs-pinentry
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

  fonts.fontconfig = {
    enable = true;
  };

  systemd.user.enable = true;
}
