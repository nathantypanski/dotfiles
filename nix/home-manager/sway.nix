{ config, pkgs, lib, mod, termFont, homeDirectory, withNixGL, ... }:


let
  # Remove debug trace for production

  swayPackageWithNixGL = pkgs.sway.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.makeWrapper ];

    postInstall = (old.postInstall or "") + ''
      # Wrap the sway binary
      wrapProgram $out/bin/sway \
        --run 'eval "$(${lib.getExe pkgs.nixgl.nixGLMesa} printenv)"'
    '';
  });

  swayPackage = if withNixGL
                then swayPackageWithNixGL
                else pkgs.sway;

  # Create user-sway based on whether nixGL is enabled
  userSway = if withNixGL == true
    then (pkgs.writeShellScriptBin "user-sway" ''
      # Set up nixGL environment and pass it to systemd-run
      exec systemd-run --user --scope \
        ${lib.getExe pkgs.nixgl.nixGLMesa} ${pkgs.sway}/bin/sway "$@"
    '')
    else (pkgs.writeShellScriptBin "user-sway" ''
      # Use plain sway
      exec systemd-run --user --scope ${pkgs.sway}/bin/sway "$@"
    '');
in
{
  options.ndt-home.sway = {
    modifier = lib.mkOption {
      type = lib.types.str;
      default = "Mod4";
      description = "Modifier key for sway shortcuts";
    };

    terminalFont = lib.mkOption {
      type = lib.types.str;
      default = "Terminus";
      description = "Terminal font name (not size!)";
    };

    withNixGL = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to wrap sway with nixGL";
    };
  };
  config = {
    home.pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name  = "Adwaita";   # any cursor theme in your icon path
      size  = 14;          # logical pixels → doubles on a 2×-scaled output
      gtk.enable  = true;  # write GTK settings files
      x11.enable  = true;  # export XCURSOR_* for XWayland & Flatpaks
      sway.enable = true;  # adds `seat * xcursor_theme …` to sway.conf
    };

    fonts.fontconfig = {
      enable = true;
    };

    programs.i3status = {
      enable = true;
      general = {
        colors = true;
        interval = 5;
      };
      modules = {
        ipv6.enable = false;
        "wireless _first_" = {
          position = 1;
          settings = {
            format_up = "W: (%quality at %essid) %ip";
            format_down = "W: down";
          };
        };
        "ethernet _first_" = {
          position = 2;
          settings = {
            format_up = "E: %ip (%speed)";
            format_down = "E: down";
          };
        };
        "battery all" = {
          position = 3;
          settings = {
            format = "%status %percentage %remaining";
          };
        };
        "disk /" = {
          position = 4;
          settings = {
            format = "%avail";
          };
        };
        load = {
          position = 5;
          settings = {
            format = "%1min";
          };
        };
        memory = {
          position = 6;
          settings = {
            format = "%used | %available";
            threshold_degraded = "1G";
            threshold_critical = "200M";
          };
        };
        "tztime local" = {
          position = 7;
          settings = {
            format = "%Y-%m-%d %H:%M:%S";
          };
        };
      };
    };

    services.mako = {
      enable = true;
      package = pkgs.mako;
      settings = {
        font = "${termFont}";
        background-color = "#3f3f3f";
        text-color = "#dcdccc";
        border-color = "#4f4f4f";
        progress-color = "over #688060";
        border-radius = 0;
        border-size = 2;
        default-timeout = 5000;
        ignore-timeout = true;
        max-visible = 5;
      };
    };

    systemd.user.services.mako = {
      Unit = {
        Description = "Mako notification daemon";
        PartOf = "sway-session.target";
        After = "sway-session.target";
      };

      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${pkgs.mako}/bin/mako";
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "sway-session.target" ];
      };
    };

    home.packages = with pkgs; [
      xwayland
      # libnotify provides `notify-send`
      libnotify
      mesa  # Graphics drivers (replaces deprecated mesa.drivers)
      wayland
      swayidle
      dconf-editor
      adwaita-icon-theme

      # sway tools (bound)
      wl-clipboard
      brightnessctl
      swayidle
      swaybg
      sway-contrib.grimshot
      swayimg

      # programs
      dconf-editor
      swayimg
      imv
      signal-desktop

      userSway
      # System swaylock wrapper (finds system binary)
      (pkgs.writeShellScriptBin "system-swaylock" ''
        exec "$(command -v swaylock || echo /usr/bin/swaylock)" "$@"
      '')
      (pkgs.writeShellScriptBin "pick-foot" ''
        exec ${pkgs.foot}/bin/foot --app-id=launcher --title=launcher \
             -e 'bash' '-c' \
                 'compgen -c |
                  grep -v fzf |
                  sort -u |
                  fzf --layout=reverse |
                  xargs -r swaymsg -t command exec'
    '')
      (pkgs.writeShellScriptBin "tmux-rebind-sway" ''
        export WAYLAND_DISPLAY="$(swaymsg -t get_outputs | jq -r '.[0].name')"
        export SWAYSOCK="$(ls /run/user/$UID/sway-ipc.* 2>/dev/null | head -n 1)"
        tmux set-environment -g WAYLAND_DISPLAY "$WAYLAND_DISPLAY"
        tmux set-environment -g SWAYSOCK "$SWAYSOCK"
    '')
    ] ++ lib.optionals withNixGL [
      nixgl.nixGLMesa
    ];

    wayland.windowManager.sway = {
      # Only use nixGL on non-nixos systems.
      package = swayPackage;
      enable = true;

      systemd = {
        enable = true;
      };
      wrapperFeatures.gtk = true;
      config = {
        window = {
          border = 3;
          hideEdgeBorders = "smart";
        };
        gaps = {
          smartBorders = "on";
          smartGaps = false;
          inner = 0;
          outer = 0;
        };
        input = {
          # framework laptop touchpad
          "2362:628:PIXA3854:00_093A:0274_Touchpad" = {
            scroll_factor = "0.5";
            accel_profile = "adaptive";
            pointer_accel = "0.1";
            dwt = "disabled";
            click_method = "clickfinger";
          };
          # external usb with trackpoint
          "1241:1031:USB-HID_Keyboard_Mouse" = {
            scroll_factor = "0.3";
            accel_profile = "adaptive";
            pointer_accel = "-1.0";
            dwt = "disabled";
          };
          "*" = {
            dwt = "disabled";
          };
        };
        terminal = "foot";
        fonts = {
          names = [termFont];
          style = "normal";
          size = 9.0;
        };
        bars = [{
          statusCommand = "i3status";
          # statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${homeDirectory}/.config/i3status-rust/config-default.toml";
          fonts = {
            names = [termFont];
            style = "normal";
            size = 9.0;
          };
          position = "top";
          colors = {
            background = "#2b2b2b";
            statusline = "#dcdccc";

            focusedWorkspace = {
              border = "#cc9393";
              background = "#cc9393";
              text = "#000000";
            };

            activeWorkspace = {
              border = "#6f6f6f";
              background = "#6f6f6f";
              text = "#dcdccc";
            };

            inactiveWorkspace = {
              border = "#3f3f3f";
              background = "#3f3f3f";
              text = "#a0a0a0";
            };

            urgentWorkspace = {
              border = "#cc9393";
              background = "#cc9393";
              text = "#3f3f3f";
            };

            bindingMode = {
              border = "#f0dfaf";
              background = "#f0dfaf";
              text = "#3f3f3f";
            };
          };
        }];
        # drag + resize with mod
        floating.modifier = "${mod}";
        keybindings = lib.attrsets.mergeAttrsList [
          (lib.attrsets.mergeAttrsList (map (num: let
            ws = toString num;
          in {
            "${mod}+${ws}" = "workspace ${ws}";
            "${mod}+Shift+${ws}" = "move container to workspace ${ws}";
          }) [1 2 3 4 5 6 7 8 9 0]))

          (lib.attrsets.concatMapAttrs (key: direction: {
            "${mod}+${key}" = "focus ${direction}";
            "${mod}+Shift+${key}" = "move ${direction}";
          }) {
            h = "left";
            j = "down";
            k = "up";
            l = "right";
          })

          {
            "${mod}+Shift+Return" = "exec --no-startup-id ${lib.getExe pkgs.foot}";

            "${mod}+Shift+c" = "kill";
            "${mod}+Shift+space" = "floating toggle";
            "${mod}+Shift+minus" = "move scratchpad";
            "${mod}+minus" = "scratchpad show";

            "${mod}+a" = "focus parent";
            "${mod}+e" = "layout toggle split";
            "${mod}+f" = "fullscreen toggle";
            "${mod}+s" = "layout stacking";
            "${mod}+w" = "layout tabbed";
            "${mod}+g" = "split h";
            "${mod}+v" = "split v";

            # recompile home-manager
            "${mod}+Shift+apostrophe" = "exec ${lib.getExe pkgs.foot} --font '${termFont}:size=9' --window-size-chars=100x50 --app-id=popup-term -- bash -i -c \"rebuild-home; read -n 1 -s -r -p '[ press any key to continue ]'\"";
            "${mod}+Shift+r" = "exec ${swayPackage}/bin/swaymsg reload";
            "--release Print" = "exec --no-startup-id ${lib.getExe pkgs.sway-contrib.grimshot} copy area";
            # Use system swaylock (Arch package) instead of Nix version
            "${mod}+Shift+semicolon" = "exec system-swaylock -f -c 3f3f3f";
            "${mod}+p" = "exec --no-startup-id pick-foot";
            "${mod}+Shift+q" = "exec ${swayPackage}/bin/swaynag -t warning -m 'Exit Sway?' -b 'Yes' '${swayPackage}/bin/swaymsg exit'";
            "XF86MonBrightnessUp" = "exec --no-startup-id ${lib.getExe pkgs.brightnessctl} s 10+";
            "XF86MonBrightnessDown" = "exec --no-startup-id ${lib.getExe pkgs.brightnessctl} s 10-";
          }
        ];
        startup = [
          { command = "${swayPackage}/bin/swaymsg workspace 1"; always = true; }
          { command = "swaybg -c #2b2b2b"; always = true; }
          { command = "${lib.getExe pkgs.swayidle} -w timeout 300 'system-swaylock -f -c 3f3f3f' before-sleep 'system-swaylock -f -c 3f3f3f'"; always = false; }
          { command = "${lib.getExe pkgs.foot} --app-id=home -e tmux new-session -A -s home -c ${homeDirectory}/src/github.com/nathantypanski/dotfiles"; always = false; }
          { command = "${lib.getExe pkgs.foot} --app-id=sys -e tmux new-session -A -s sys"; always = false; }
          { command = "${lib.getExe pkgs.foot} --app-id=mon -e bash -c 'tmux new-session -A -s mon \\; send-keys htop Enter'"; always = false; }
          { command = "${lib.getExe config.ndt-home.firefox-jailed}"; always = false; }
          { command = "${lib.getExe pkgs.foot} --app-id=scratchpad -e tmux new-session -A -s scratch"; always = false; }
        ];
      };
      extraConfig = ''
      input "type:keyboard" {
        xkb_layout us
        xkb_options caps:ctrl_modifier
        repeat_delay 333
        repeat_rate 25
      }
      # to list options do `swaymsg -t get_outputs`
      output eDP-1 scale 2
      for_window [app_id="popup-term"] floating enable, move position center, border pixel 3
      for_window [app_id="launcher"] floating enable, move position center, border pixel 3, focus
      for_window [app_id="home"] move to workspace 1
      for_window [app_id="sys"] move to workspace 2
      for_window [app_id="mon"] move to workspace 0
      for_window [app_id="scratchpad"] move scratchpad
      workspace_auto_back_and_forth yes
      default_border pixel 1
      default_floating_border pixel 3
      titlebar_border_thickness 0
    '';
      extraSessionCommands = ''
      # Hardware rendering workaround for Arch (commented out - using nixGL instead)
      # export WLR_RENDERER=pixman
      # export LIBGL_DRIVERS_PATH=/usr/lib/dri
      # export GBM_DRIVERS_PATH=/usr/lib/dri
    '';
    };
  };
}
