{ config, pkgs, lib, mod, termFont, homeDirectory, withNixGL, ... }:

let
  # Create nixGL-wrapped river package if needed
  riverPackage = if withNixGL
    then pkgs.writeShellScriptBin "river" ''
      exec ${lib.getExe pkgs.nixgl.nixGLMesa} ${lib.getExe pkgs.river-classic} "$@"
    ''
    else pkgs.river-classic;

  # Create user-river script - the built-in module handles session management automatically
  userRiver = pkgs.writeShellScriptBin "user-river" ''
    echo "Starting River compositor with session management..."

    # The built-in river module will automatically:
    # 1. Stop river-session.target
    # 2. Start river-session.target (which starts mako, river-scale, etc.)
    # 3. Start river with proper nixGL integration (if enabled)

    exec ${lib.getExe riverPackage}
  '';

in {

  # Add fuzzel configuration
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
    programs.fuzzel = {
      enable = true;
      settings = {
        main = {
          font = "DepartureMono Nerd Font:size=12";
          dpi-aware = "yes";
          show-actions = "yes";
          terminal = "${pkgs.foot}/bin/foot";

          # Appearance
          width = 40;
          horizontal-pad = 20;
          vertical-pad = 10;
          inner-pad = 10;
        };

        colors = {
          background = "121212ff";
          text = "dcdcccff";
          match = "60b48aff";
          selection = "3f3f3fff";
          selection-match = "dfaf8fff";
          selection-text = "dcdcccff";
          border = "506070ff";
        };

        border = {
          width = 2;
          radius = 0;
        };
      };
    };
    # Enable River wayland compositor using built-in Home Manager module
    wayland.windowManager.river = {
      enable = true;
      package = riverPackage;  # Use nixGL-wrapped package if withNixGL is true

      # Enable systemd integration
      systemd = {
        enable = true;
        # Default extraCommands already handle river-session.target:
        # - Stop river-session.target
        # - Start river-session.target
        # This automatically manages our mako and river-scale services!
      };

      # Enable Xwayland support
      xwayland = {
        enable = true;
      };

      # River configuration
      extraConfig = ''
      # Debug logging
      echo "$(date): River config starting" >> /tmp/river-debug.log

      # Start rivertile layout generator in background
      echo "$(date): Starting rivertile" >> /tmp/river-debug.log
      ${pkgs.river-classic}/bin/rivertile -view-padding 2 -outer-padding 1 &

      # Set rivertile as default layout
      echo "$(date): Setting default layout" >> /tmp/river-debug.log
      ${pkgs.river-classic}/bin/riverctl default-layout rivertile

      # waybar and swayidle are now managed by systemd services
      echo "$(date): Services (waybar, swayidle, scaling) managed by river-session.target" >> /tmp/river-debug.log

      # Launch initial foot terminal for debugging
      echo "$(date): Starting debug terminal" >> /tmp/river-debug.log
      ${pkgs.foot}/bin/foot &

      echo "$(date): Setting up rules and scaling" >> /tmp/river-debug.log
      ${pkgs.river-classic}/bin/riverctl rule-add -app-id "foot" ssd
      ${pkgs.river-classic}/bin/riverctl rule-add -app-id "launcher" float
      ${pkgs.river-classic}/bin/riverctl rule-add -app-id "yazi-popup" float
      ${pkgs.river-classic}/bin/riverctl rule-add -title "rebuild-home" tags 512

      # New windows spawn on focused tags only (not all visible tags)
      ${pkgs.river-classic}/bin/riverctl spawn-tagmask 0

      # Remap Caps Lock to Control
      ${pkgs.river-classic}/bin/riverctl keyboard-layout -options ctrl:nocaps us

      # Mouse/pointer settings
      ${pkgs.river-classic}/bin/riverctl input pointer accel-profile adaptive
      ${pkgs.river-classic}/bin/riverctl input pointer pointer-accel 0.5

      # Touchpad settings - Framework laptop
      ${pkgs.river-classic}/bin/riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" tap disabled
      ${pkgs.river-classic}/bin/riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" tap-button-map lrm
      ${pkgs.river-classic}/bin/riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" click-method clickfinger
      ${pkgs.river-classic}/bin/riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" scroll-method two-finger
      ${pkgs.river-classic}/bin/riverctl input "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad" scroll-factor 0.3

      # Basic keybinds
      echo "$(date): Setting up keybindings" >> /tmp/river-debug.log
      ${pkgs.river-classic}/bin/riverctl map normal Super Return spawn "${pkgs.foot}/bin/foot"
      ${pkgs.river-classic}/bin/riverctl map normal Super P spawn pick-ruby
      ${pkgs.river-classic}/bin/riverctl map normal Super W spawn wifi-menu
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift P spawn system-menu
      ${pkgs.river-classic}/bin/riverctl map normal Super Tab spawn window-menu
      ${pkgs.river-classic}/bin/riverctl map normal Super Q close
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift E exit
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift T toggle-float
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift semicolon spawn system-swaylock
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift apostrophe spawn "${pkgs.foot}/bin/foot --title=rebuild-home -e rebuild-home"
      ${pkgs.river-classic}/bin/riverctl map normal Super Y spawn yazi-popup
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift R spawn "sh ~/.config/river/init"
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift S spawn "wlr-randr --output eDP-1 --scale 2.0"
      ${pkgs.river-classic}/bin/riverctl map normal Super space spawn "notify-send -t 1000 'River' 'Tag Mode: Press 0-9' && ${pkgs.river-classic}/bin/riverctl enter-mode tag"

      # Brightness controls
      ${pkgs.river-classic}/bin/riverctl map normal None XF86MonBrightnessUp spawn "${pkgs.brightnessctl}/bin/brightnessctl s '+5%'"
      ${pkgs.river-classic}/bin/riverctl map normal None XF86MonBrightnessDown spawn "${pkgs.brightnessctl}/bin/brightnessctl s '5%-'"

      # Focus controls
      ${pkgs.river-classic}/bin/riverctl map normal Super J focus-view next
      ${pkgs.river-classic}/bin/riverctl map normal Super K focus-view previous
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift J swap next
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift K swap previous

      # Layout controls - for rivertile
      ${pkgs.river-classic}/bin/riverctl map normal Super H send-layout-cmd rivertile "main-ratio -0.05"
      ${pkgs.river-classic}/bin/riverctl map normal Super L send-layout-cmd rivertile "main-ratio +0.05"
      ${pkgs.river-classic}/bin/riverctl map normal Super I send-layout-cmd rivertile "main-count +1"
      ${pkgs.river-classic}/bin/riverctl map normal Super D send-layout-cmd rivertile "main-count -1"

      # Layout orientation
      ${pkgs.river-classic}/bin/riverctl map normal Super V send-layout-cmd rivertile "main-location left"
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift V send-layout-cmd rivertile "main-location right"
      ${pkgs.river-classic}/bin/riverctl map normal Super B send-layout-cmd rivertile "main-location top"
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift B send-layout-cmd rivertile "main-location bottom"

      # Switch layout generators
      ${pkgs.river-classic}/bin/riverctl map normal Super M spawn "pkill rivertile; ${pkgs.river-classic}/bin/rivertile -view-padding 2 -outer-padding 1 &"
      ${pkgs.river-classic}/bin/riverctl map normal Super+Shift M spawn "riverctl default-layout rivertile"

      # Tag operations - Super+0-9 for quick access
      for i in $(seq 0 9); do
            tags=$(( i == 0 ? (1 << 9) : (1 << (i - 1)) ))

            # Switch to tag
            ${pkgs.river-classic}/bin/riverctl map normal "Super" "$i" toggle-focused-tags "$tags"

            # Move window to tag
            ${pkgs.river-classic}/bin/riverctl map normal "Super+Shift" "$i" set-view-tags "$tags"

            # Switch to tag
            ${pkgs.river-classic}/bin/riverctl map normal "Super+Alt" "$i" set-focused-tags "$tags"

      done

      # Declare modes
      ${pkgs.river-classic}/bin/riverctl declare-mode tag
      ${pkgs.river-classic}/bin/riverctl declare-mode layout

      # Tag mode - for moving windows to tags
      ${pkgs.river-classic}/bin/riverctl map normal Super T enter-mode tag
      ${pkgs.river-classic}/bin/riverctl map tag None Escape enter-mode normal

      # Layout mode - for layout operations
      ${pkgs.river-classic}/bin/riverctl map normal Super G enter-mode layout
      ${pkgs.river-classic}/bin/riverctl map layout None h send-layout-cmd rivertile "main-ratio -0.05"
      ${pkgs.river-classic}/bin/riverctl map layout None l send-layout-cmd rivertile "main-ratio +0.05"
      ${pkgs.river-classic}/bin/riverctl map layout None i send-layout-cmd rivertile "main-count +1"
      ${pkgs.river-classic}/bin/riverctl map layout None d send-layout-cmd rivertile "main-count -1"
      ${pkgs.river-classic}/bin/riverctl map layout None v send-layout-cmd rivertile "main-location left"
      ${pkgs.river-classic}/bin/riverctl map layout None V send-layout-cmd rivertile "main-location right"
      ${pkgs.river-classic}/bin/riverctl map layout None b send-layout-cmd rivertile "main-location top"
      ${pkgs.river-classic}/bin/riverctl map layout None B send-layout-cmd rivertile "main-location bottom"
      ${pkgs.river-classic}/bin/riverctl map layout None Escape enter-mode normal

      # Mouse bindings
      # ${pkgs.river-classic}/bin/riverctl map-pointer normal Super BTN_LEFT move-view
      ${pkgs.river-classic}/bin/riverctl map-pointer normal Super BTN_RIGHT resize-view

      # Set repeat rate
      ${pkgs.river-classic}/bin/riverctl set-repeat 50 300

      # Cursor theme
      ${pkgs.river-classic}/bin/riverctl xcursor-theme ${config.gtk.cursorTheme.name or "Adwaita"} ${toString (config.gtk.cursorTheme.size or 12)}

      # Set background color to match waybar
      ${pkgs.river-classic}/bin/riverctl background-color 0x121212

      echo "$(date): River config completed" >> /tmp/river-debug.log
    '';
    };

    programs.waybar = {
      enable = true;
      systemd = {
        enable = true;
        target = "river-session.target";
      };
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          # minimum height required by waybar modules
          height = 28;
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
            states = {
              warning = 30;
              critical = 15;
            };
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

    # Additional services that integrate with built-in river-session.target

    systemd.user.services.mako = {
      Unit = {
        Description = "Mako notification daemon";
        PartOf = [ "river-session.target" ];
        After = [ "river-session.target" ];
      };

      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${pkgs.mako}/bin/mako";
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "river-session.target" ];
      };
    };

# waybar systemd integration is handled automatically by programs.waybar.enable

    systemd.user.services.river-scale = {
      Unit = {
        Description = "Apply River display scaling";
        PartOf = [ "river-session.target" ];
        After = [ "river-session.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.wlr-randr}/bin/wlr-randr --output eDP-1 --scale 2.0";
        RemainAfterExit = true;
      };
      Install = {
        WantedBy = [ "river-session.target" ];
      };
    };

    systemd.user.services.swayidle = {
      Unit = {
        Description = "swayidle - idle management daemon for Wayland";
        PartOf = [ "river-session.target" ];
        After = [ "river-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.swayidle}/bin/swayidle -w timeout 600 'system-swaylock -f -c 3f3f3f' before-sleep 'system-swaylock -f -c 3f3f3f'";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "river-session.target" ];
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
                  xargs -r ${pkgs.river-classic}/bin/riverctl spawn'
    '')
      (pkgs.writeShellScriptBin "yazi-popup" ''
        exec ${pkgs.foot}/bin/foot --app-id=yazi-popup --title=yazi-popup \
             -e ${pkgs.yazi}/bin/yazi
    '')

      fuzzel

      # Ruby environment
      ruby
      rubyPackages.sorbet-runtime
      (pkgs.writeShellScriptBin "setup-ruby-env" ''
      gem install sorbet sorbet-runtime --user-install
    '')

      # Ruby script binaries using readFile from bin directory
      (pkgs.writeScriptBin "launcher.rb" ''
        #!${lib.getExe pkgs.ruby}
        ${builtins.readFile ./../../../bin/launcher.rb}
      '')

      (pkgs.writeScriptBin "wifi-menu.rb" ''
        #!${lib.getExe pkgs.ruby}
        ${builtins.readFile ./../../../bin/wifi-menu.rb}
      '')

      (pkgs.writeScriptBin "system-menu.rb" ''
        #!${lib.getExe pkgs.ruby}
        ${builtins.readFile ./../../../bin/system-menu.rb}
      '')

      # Ruby-powered launchers using the script binaries
      (pkgs.writeShellScriptBin "pick-ruby" ''
      export GEM_PATH="${pkgs.rubyPackages.sorbet-runtime}/${pkgs.ruby.gemPath}"
      launcher.rb | fuzzel --dmenu | xargs -r ${pkgs.river-classic}/bin/riverctl spawn
    '')

      (pkgs.writeShellScriptBin "wifi-menu" ''
      export GEM_PATH="${pkgs.rubyPackages.sorbet-runtime}/${pkgs.ruby.gemPath}"
      selected=$(wifi-menu.rb | fuzzel --dmenu --prompt="WiFi: ")
      if [ -n "$selected" ]; then
        ${pkgs.river-classic}/bin/riverctl spawn "foot -e iwctl --passphrase-command 'read -s -p \"Password: \" pwd && echo \$pwd' station wlan0 connect '$selected'"
      fi
    '')

      (pkgs.writeShellScriptBin "system-menu" ''
      export GEM_PATH="${pkgs.rubyPackages.sorbet-runtime}/${pkgs.ruby.gemPath}"
      selected=$(system-menu.rb | fuzzel --dmenu --prompt="System: ")
      if [ -n "$selected" ]; then
        system-menu.rb "$selected"
      fi
    '')

      # Quick window/app switcher
      (pkgs.writeShellScriptBin "window-menu" ''
      ${pkgs.river-classic}/bin/riverctl spawn "fuzzel"
    '')
    ] ++ lib.optionals withNixGL [
      nixgl.nixGLMesa
    ];
  };
}
