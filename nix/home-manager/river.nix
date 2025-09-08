{ config, pkgs, lib, mod, termFont, homeDirectory, withNixGL, ... }:

let
  # Create user-river based on whether nixGL is enabled
  userRiver = if withNixGL == true
    then (pkgs.writeShellScriptBin "user-river" ''
      # Clear conflicting GL paths before nixGL sets its own
      unset LIBGL_DRIVERS_PATH LD_LIBRARY_PATH __EGL_VENDOR_LIBRARY_FILENAMES LIBVA_DRIVERS_PATH GBM_BACKENDS_PATH
      exec systemd-run --user --scope \
        ${lib.getExe pkgs.nixgl.nixGLMesa} ${lib.getExe pkgs.river-classic} "$@"
    '')
    else (pkgs.writeShellScriptBin "user-river" ''
      # Use plain river
      exec systemd-run --user --scope ${lib.getExe pkgs.river-classic} "$@"
    '');
in
{
  options.ndt-home.river = {
    modifier = lib.mkOption {
      type = lib.types.str;
      default = "Mod4";
      description = "Modifier key for river shortcuts";
    };

    terminalFont = lib.mkOption {
      type = lib.types.str;
      default = "Terminus";
      description = "Terminal font name (not size!)";
    };

    withNixGL = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to wrap river with nixGL";
    };
  };
  config = {

    # Enable River wayland compositor
    wayland.windowManager.river = {
      enable = true;
      package = pkgs.river-classic;

      # River configuration
      extraConfig = ''
      # Start rivertile layout generator in background
      rivertile -view-padding 2 -outer-padding 1 &

      # Set rivertile as default layout
      riverctl default-layout rivertile

      # Start waybar
      pkill waybar >& /dev/null; waybar &

      # Display scaling - 2x zoom for most apps, normal for foot
      riverctl rule-add -app-id "foot" ssd
      riverctl rule-add -app-id "launcher" float
      riverctl rule-add -title "rebuild-home" tags 512

      # New windows spawn on focused tags only (not all visible tags)
      riverctl spawn-tagmask 0
      riverctl spawn "wlr-randr --output eDP-1 --scale 2.0"

      # Remap Caps Lock to Control
      riverctl keyboard-layout -options ctrl:nocaps us

      # Mouse/pointer settings
      riverctl input pointer accel-profile adaptive
      riverctl input pointer pointer-accel 0.5

      # Touchpad settings - Framework laptop
      riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" tap enabled
      riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" tap-button-map lrm
      riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" click-method clickfinger
      riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" scroll-method two-finger
      riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" scroll-factor 0.3

      # Basic keybinds
      riverctl map normal Super Return spawn "${pkgs.foot}/bin/foot"
      riverctl map normal Super P spawn pick-foot
      riverctl map normal Super Q close
      riverctl map normal Super+Shift E exit
      riverctl map normal Super+Shift T toggle-float
      riverctl map normal Super+Shift semicolon spawn system-swaylock
      riverctl map normal Super+Shift apostrophe spawn "${pkgs.foot}/bin/foot --title=rebuild-home -e rebuild-home"
      riverctl map normal Super+Shift R spawn "sh ~/.config/river/init"
      riverctl map normal Super+Shift S spawn "wlr-randr --output eDP-1 --scale 2.0"
      riverctl map normal Super space spawn "notify-send -t 1000 'River' 'Tag Mode: Press 0-9' && riverctl enter-mode tag"

      # Brightness controls
      riverctl map normal None XF86MonBrightnessUp spawn "${pkgs.brightnessctl}/bin/brightnessctl s '+5%'"
      riverctl map normal None XF86MonBrightnessDown spawn "${pkgs.brightnessctl}/bin/brightnessctl s '5%-'"

      # Focus controls
      riverctl map normal Super J focus-view next
      riverctl map normal Super K focus-view previous
      riverctl map normal Super+Shift J swap next
      riverctl map normal Super+Shift K swap previous

      # Layout controls - for rivertile
      riverctl map normal Super H send-layout-cmd rivertile "main-ratio -0.05"
      riverctl map normal Super L send-layout-cmd rivertile "main-ratio +0.05"
      riverctl map normal Super I send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super D send-layout-cmd rivertile "main-count -1"

      # Layout orientation
      riverctl map normal Super V send-layout-cmd rivertile "main-location left"
      riverctl map normal Super+Shift V send-layout-cmd rivertile "main-location right"
      riverctl map normal Super B send-layout-cmd rivertile "main-location top"
      riverctl map normal Super+Shift B send-layout-cmd rivertile "main-location bottom"

      # Switch layout generators
      riverctl map normal Super M spawn "pkill rivertile; rivertile -view-padding 2 -outer-padding 1 &"
      riverctl map normal Super+Shift M spawn "riverctl default-layout rivertile"

      # Tag operations - Super+0-9 for quick access
      for i in $(seq 0 9); do
            tags=$(( i == 0 ? (1 << 9) : (1 << (i - 1)) ))

            # Switch to tag
            riverctl map normal "Super" "$i" toggle-focused-tags "$tags"

            # Move window to tag
            riverctl map normal "Super+Shift" "$i" set-view-tags "$tags"

            # Switch to tag
            riverctl map normal "Super+Alt" "$i" set-focused-tags "$tags"

      done

      # Declare modes
      riverctl declare-mode tag
      riverctl declare-mode layout

      # Tag mode - for moving windows to tags
      riverctl map normal Super T enter-mode tag
      riverctl map tag None Escape enter-mode normal

      # Layout mode - for layout operations
      riverctl map normal Super G enter-mode layout
      riverctl map layout None h send-layout-cmd rivertile "main-ratio -0.05"
      riverctl map layout None l send-layout-cmd rivertile "main-ratio +0.05"
      riverctl map layout None i send-layout-cmd rivertile "main-count +1"
      riverctl map layout None d send-layout-cmd rivertile "main-count -1"
      riverctl map layout None v send-layout-cmd rivertile "main-location left"
      riverctl map layout None V send-layout-cmd rivertile "main-location right"
      riverctl map layout None b send-layout-cmd rivertile "main-location top"
      riverctl map layout None B send-layout-cmd rivertile "main-location bottom"
      riverctl map layout None Escape enter-mode normal

      # Mouse bindings
      # riverctl map-pointer normal Super BTN_LEFT move-view
      riverctl map-pointer normal Super BTN_RIGHT resize-view

      # Set repeat rate
      riverctl set-repeat 50 300

      # Cursor theme
      riverctl xcursor-theme ${config.gtk.cursorTheme.name or "Adwaita"} ${toString (config.gtk.cursorTheme.size or 12)}

      # Set background color to match waybar
      riverctl background-color 0x121212
    '';
    };

    programs.waybar = {
      enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 24;
          modules-left = [ "river/tags" "river/mode" ];
          modules-center = [ "river/window" ];
          modules-right = [ "memory" "clock" "battery" "network" ];

          "river/tags" = {
            num-tags = 10;
            tag-labels = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ];
          };

          "river/window" = {
            format = " {}";
            max-length = 50;
          };

          "river/mode" = {
            format = "<span style=\"italic\">{}</span>";
            tooltip = false;
          };

          memory = {
            format = "󰍛 {}%";
            tooltip-format = "Memory: {used:0.1f}GiB / {total:0.1f}GiB";
            interval = 5;
          };

          clock = {
            format = "󰥔 {:%Y-%m-%d %H:%M:%S}";
            interval = 1;
            tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          };

          battery = {
            format = "{icon} {capacity}%";
            format-icons = [ "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹" ];
            format-charging = "󰂄 {capacity}%";
            format-plugged = "󰂄 {capacity}%";
          };

          network = {
            format-wifi = "󰤨 {signalStrength}%";
            format-ethernet = "󰈀 Connected";
            format-disconnected = "󰤭 Disconnected";
            tooltip-format = "{ifname}: {ipaddr}";
          };
        };
      };
      style = ''
        * {
          font-family: "DepartureMono Nerd Font", monospace;
          font-size: 12px;
        }
        window#waybar {
          background-color: #121212;
          border-bottom: 2px solid #3f3f3f;
          color: #dcdccc;
          padding: 0 2px;
        }
        .modules-left, .modules-center, .modules-right {
          margin: 0 5px;
        }
        .modules-left > * {
          margin: 0 5px 0 0;
        }
        .modules-center > * {
          margin: 0 5px;
        }
        .modules-right > * {
          margin: 0 0 0 5px;
        }
        #tags {
          margin: 0 10px 0 0;
        }
        #tags button {
          background: #3f3f3f;
          color: #709080;
          border: 1px solid #506070;
          padding: 1px 4px;
          margin: 0 1px;
          border-radius: 2px;
          min-width: 14px;
        }
        #tags button.occupied {
          background: #506070;
          color: #94bff3;
        }
        #tags button.focused {
          background: #60b48a;
          color: #121212;
        }
        #window {
          margin: 0 10px;
          color: #dfaf8f;
        }
        #memory {
          margin: 0 5px 0 10px;
          color: #f0dfaf;
          border-right: 1px solid #3f3f3f;
          padding-right: 8px;
        }
        #clock {
          margin: 0 5px;
          color: #dc8cc3;
          border-right: 1px solid #3f3f3f;
          padding-right: 8px;
        }
        #battery {
          margin: 0 5px;
          color: #60b48a;
          border-right: 1px solid #3f3f3f;
          padding-right: 8px;
        }
        #battery.charging {
          color: #dfaf8f;
        }
        #battery.critical {
          color: #705050;
        }
        #network {
          margin: 0 5px 0 0;
          color: #8cd0d3;
        }
      '';
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
    systemd.user.services.river-scale = {
      Unit = {
        Description = "Apply River display scaling";
        After = "suspend.target";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.wlr-randr}/bin/wlr-randr --output eDP-1 --scale 2.0";
        RemainAfterExit = true;
      };
      Install = {
        WantedBy = [ "suspend.target" ];
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

      # river/wayland tools
      wl-clipboard
      brightnessctl
      wlr-randr  # For display scaling
      sway-contrib.grimshot

      # programs
      dconf-editor
      swayimg
      imv
      signal-desktop

      userRiver
      (pkgs.writeShellScriptBin "emacs-tiling" ''
        exec emacs --batch -l "${homeDirectory}/.config/river/layout.el"
      '')
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
                  xargs -r riverctl spawn'
    '')
    ] ++ lib.optionals withNixGL [
      nixgl.nixGLMesa
    ];
  };
}
