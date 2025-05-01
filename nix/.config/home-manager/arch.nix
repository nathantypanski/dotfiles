{ config, pkgs, lib, username, homeDirectory, secrets, ... }:

let
  copyCommand = "wl-copy";
  mod = "Mod4";
in {
    home.username = username;
    home.homeDirectory = homeDirectory;

  imports = [
    ./zsh.nix
    (import ./tmux.nix { inherit config pkgs copyCommand; })
    (import ./neovim.nix { inherit config pkgs; })
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
    dconf-editor
    sway-contrib.grimshot
    swaybg
    fzf
    bashInteractive

    (pkgs.writeShellScriptBin "pick-foot" ''
      exec ${pkgs.foot}/bin/foot --app-id=launcher --title=launcher -e 'bash' '-c' 'compgen -c | grep -v fzf | sort -u | fzf --layout=reverse | xargs -r swaymsg -t command exec'
    '')

  ];

  programs.wofi = {
    enable = true;
    settings = {
      font = "Terminus:size=12";
    };
  };

  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Terminus:size=10";
      };
      colors = {
        foreground = "dcdccc";
        background = "121212";
        selection-foreground = "3f3f3f";
        selection-background = "dcdccc";

        # ANSI 0–7
        regular0  = "3f3f3f";  # black
        regular1  = "705050";  # red
        regular2  = "60b48a";  # green
        regular3  = "dfaf8f";  # yellow
        regular4  = "506070";  # blue
        regular5  = "dc8cc3";  # magenta
        regular6  = "8cd0d3";  # cyan
        regular7  = "dcdccc";  # white (light gray)

        # Bright 8–15
        bright0   = "709080";  # bright black (gray)
        bright1   = "dca3a3";  # bright red
        bright2   = "c3bf9f";  # bright green
        bright3   = "f0dfaf";  # bright yellow
        bright4   = "94bff3";  # bright blue
        bright5   = "ec93d3";  # bright magenta
        bright6   = "93e0e3";  # bright cyan
        bright7   = "ffffff";  # bright white
      };
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
    # Put session vars here
  };
}
