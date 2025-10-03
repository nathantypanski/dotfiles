{ config, pkgs, lib, mod, termFont, homeDirectory, withNixGL, ... }:

let
  # Ruby configuration
  rubyVersion = pkgs.ruby_3_4;
  rubyEnv = pkgs.bundlerEnv {
    name = "river-ruby-env";
    ruby = rubyVersion;
    gemdir = ./gems;
  };

  # Common binaries
  riverctl = "${pkgs.river-classic}/bin/riverctl";
  fuzzelBin = "${pkgs.fuzzel}/bin/fuzzel";
  foot = "${lib.getExe pkgs.foot}";
  iwctl = "${pkgs.iwd}/bin/iwctl";

  # Ruby script paths as Nix variables
  launcherScript = pkgs.writeScriptBin "launcher.rb" ''
    ${builtins.readFile ./../../bin/launcher.rb}
  '';

  wifiMenuScript = pkgs.writeScriptBin "wifi-menu.rb" ''
    ${builtins.readFile ./../../bin/wifi-menu.rb}
  '';

  systemMenuScript = pkgs.writeScriptBin "system-menu.rb" ''
    ${builtins.readFile ./../../bin/system-menu.rb}
  '';

  # Helper to run Ruby scripts with the bundlerEnv
  runRubyScript = script: ''
    export PATH="${rubyEnv}/bin:$PATH"
    export GEM_PATH="${rubyEnv}/${rubyEnv.ruby.gemPath}:$GEM_PATH"
    ${lib.getExe rubyVersion} "${script}/bin/${script.name}" "$@"
  '';

  # Fuzzel menu helper
  fuzzelMenu = prompt:
    if prompt != "" then
      "${fuzzelBin} --dmenu --prompt=\"${prompt}\""
    else
      "${fuzzelBin} --dmenu";

  # Create a launcher that pipes to fuzzel and spawns with riverctl
  mkLauncher = { name, script, prompt ? "" }: pkgs.writeShellScriptBin name ''
    export PATH="${rubyEnv}/bin:$PATH"
    export GEM_PATH="${rubyEnv}/${rubyEnv.ruby.gemPath}:$GEM_PATH"
    ${lib.getExe rubyVersion} "${script}/bin/${script.name}" | ${fuzzelMenu prompt} | xargs -r ${riverctl} spawn
  '';

  # Create a menu selector with custom action
  mkMenuSelector = { name, script, prompt ? "", action }: pkgs.writeShellScriptBin name ''
    export PATH="${rubyEnv}/bin:$PATH"
    export GEM_PATH="${rubyEnv}/${rubyEnv.ruby.gemPath}:$GEM_PATH"
    selected=$(${lib.getExe rubyVersion} "${script}/bin/${script.name}" | ${fuzzelMenu prompt})
    if [ -n "$selected" ]; then
      ${action}
    fi
  '';

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
          terminal = foot;

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
          radius = 2;
        };
      };
    };
    # Enable River wayland compositor using built-in Home Manager module
    wayland.windowManager.river = {
      enable = true;
      package = riverPackage;

      # Enable systemd integration for mako, river-scale
      systemd = {
        enable = true;
      };

      xwayland = {
        enable = true;
      };

      extraConfig = ''
      logfile='/tmp/river-debug.log'
      term='${foot}'
      layoutBin='${pkgs.river-classic}/bin/rivertile'
      riverctl='${riverctl}'

      scratchpadTag=$(( 1 << 31 ))

      log() {
            echo <&2 "$(date): " "$@" | tee -a "$logfile"
      }

      # Debug logging
      log "River config starting"

      # Start the systemd river-session.target
      systemctl --user start river-session.target &

      # Start rivertile layout generator in background
      log "Starting rivertile"
      log "Command: $layoutBin -view-padding 2 -outer-padding 1"
      nohup "$layoutBin" -view-padding 2 -outer-padding 1 >/dev/null 2>&1 &
      sleep 0.1  # Give rivertile time to start

      # Set rivertile as default layout
      log "Setting default layout"
      "$riverctl" default-layout rivertile

      # waybar and swayidle are now managed by systemd services
      log "Services (waybar, swayidle, scaling) managed by river-session.target"

      log "Setting up rules and scaling"
      "$riverctl" rule-add -app-id "foot" ssd
      "$riverctl" rule-add -app-id "launcher" float
      "$riverctl" rule-add -app-id "yazi-popup" float
      "$riverctl" rule-add -title "rebuild-river" tags 512

      # Make Firefox use server-side decorations (i.e., via tiling WM)
      "$riverctl" rule-add -app-id "firefox*" ssd
      "$riverctl" rule-add -title "*Firefox*" ssd


      # New windows spawn on focused tags only (not all visible tags)
      "$riverctl" spawn-tagmask 0

      # Remap Caps Lock to Control
      "$riverctl" keyboard-layout -options ctrl:nocaps us

      # Mouse/pointer settings
      "$riverctl" input pointer accel-profile adaptive
      "$riverctl" input pointer pointer-accel 0.5

      # Touchpad settings - Framework laptop
      pointer="pointer-2362-628-PIXA3854:00_093A:0274_Touchpad"
      "$riverctl" input "$pointer" tap disabled
      "$riverctl" input "$pointer" tap-button-map lrm
      "$riverctl" input "$pointer" click-method clickfinger
      "$riverctl" input "$pointer" scroll-method two-finger
      "$riverctl" input "$pointer" scroll-factor 0.3

      # Basic keybinds
      log "Setting up keybindings"
      "$riverctl" map normal Super Return spawn "$term"
      "$riverctl" map normal Super P spawn pick-ruby
      "$riverctl" map normal Super W spawn wifi-menu
      "$riverctl" map normal Super+Shift P spawn system-menu
      "$riverctl" map normal Super Tab spawn window-menu
      "$riverctl" map normal Super Q close
      "$riverctl" map normal Super+Shift E exit
      "$riverctl" map normal Super+Shift Space toggle-float
      "$riverctl" map normal Super F toggle-fullscreen
      "$riverctl" map normal Super+Shift semicolon spawn system-swaylock
      "$riverctl" map normal Super+Shift apostrophe spawn "$term --title=rebuild-river -e 'rebuild-river'"
      "$riverctl" map normal Super Y spawn yazi-popup
      "$riverctl" map normal Super+Shift R spawn "sh ~/.config/river/init"
      "$riverctl" map normal Super+Shift S spawn "wlr-randr --output eDP-1 --scale 2.0"

      # Scratchpad functionality (like Sway)
      "$riverctl" map normal Super+Shift minus set-view-tags "$scratchpadTag" # Move window to scratchpad
      "$riverctl" map normal Super minus toggle-focused-tags "$scratchpadTag" # Focus scratchpad

      # Brightness controls
      "$riverctl" map normal None XF86MonBrightnessUp spawn "${lib.getExe pkgs.brightnessctl} s '+5%'"
      "$riverctl" map normal None XF86MonBrightnessDown spawn "${lib.getExe pkgs.brightnessctl} s '5%-'"

      # Focus controls
      "$riverctl" map normal Super J focus-view next
      "$riverctl" map normal Super K focus-view previous
      "$riverctl" map normal Super+Shift J swap next
      "$riverctl" map normal Super+Shift K swap previous

      # Layout controls - for rivertile
      "$riverctl" map normal Super H send-layout-cmd rivertile "main-ratio -0.05"
      "$riverctl" map normal Super L send-layout-cmd rivertile "main-ratio +0.05"
      "$riverctl" map normal Super I send-layout-cmd rivertile "main-count +1"
      "$riverctl" map normal Super D send-layout-cmd rivertile "main-count -1"

      # Layout orientation
      "$riverctl" map normal Super V send-layout-cmd rivertile "main-location left"
      "$riverctl" map normal Super+Shift V send-layout-cmd rivertile "main-location right"
      "$riverctl" map normal Super B send-layout-cmd rivertile "main-location top"
      "$riverctl" map normal Super+Shift B send-layout-cmd rivertile "main-location bottom"

      # Switch layout generators
      "$riverctl" map normal Super M spawn "pkill rivertile; ${riverPackage}/bin/rivertile -view-padding 2 -outer-padding 1 &"
      "$riverctl" map normal Super+Shift M spawn "$riverctl default-layout rivertile"

      # Tag operations - Super+0-9 for quick access
      for i in $(seq 0 9); do
            tags=$(( i == 0 ? (1 << 9) : (1 << (i - 1)) ))

            # Switch to tag
            "$riverctl" map normal "Super" "$i" toggle-focused-tags "$tags"

            # Move window to tag
            "$riverctl" map normal "Super+Shift" "$i" set-view-tags "$tags"

            # Switch to tag
            "$riverctl" map normal "Super+Alt" "$i" set-focused-tags "$tags"

      done

      # Declare modes
      "$riverctl" declare-mode tag
      "$riverctl" declare-mode layout

      # Tag mode - for moving windows to tags
      "$riverctl" map normal Super T enter-mode tag
      "$riverctl" map tag None Escape enter-mode normal

      "$riverctl" map normal Super space spawn "sh -c 'notify-send -t 1000 \"River\" \"Tag Mode: Press 0-9\"; $riverctl enter-mode tag'"
      # Tag mode bindings (0-9 for tag operations)
      for i in $(seq 0 9); do
            tags=$(( i == 0 ? (1 << 9) : (1 << (i - 1)) ))

            # In tag mode: numbers switch to tags
            "$riverctl" map tag None "$i" set-focused-tags "$tags"
            # In tag mode: Shift+numbers move window to tag
            "$riverctl" map tag Shift "$i" set-view-tags "$tags"
      done

      # Layout mode - for layout operations
      "$riverctl" map normal Super G enter-mode layout
      "$riverctl" map layout None h send-layout-cmd rivertile "main-ratio -0.05"
      "$riverctl" map layout None l send-layout-cmd rivertile "main-ratio +0.05"
      "$riverctl" map layout None i send-layout-cmd rivertile "main-count +1"
      "$riverctl" map layout None d send-layout-cmd rivertile "main-count -1"
      "$riverctl" map layout None v send-layout-cmd rivertile "main-location left"
      "$riverctl" map layout None V send-layout-cmd rivertile "main-location right"
      "$riverctl" map layout None b send-layout-cmd rivertile "main-location top"
      "$riverctl" map layout None B send-layout-cmd rivertile "main-location bottom"
      "$riverctl" map layout None Escape enter-mode normal

      # Mouse bindings
      "$riverctl" map-pointer normal Super BTN_LEFT move-view
      "$riverctl" map-pointer normal Super BTN_RIGHT resize-view

      # Set repeat rate
      "$riverctl" set-repeat 50 300

      # Cursor theme
      "$riverctl" xcursor-theme ${config.gtk.cursorTheme.name or "Adwaita"} ${toString (config.gtk.cursorTheme.size or 12)}

      # Set background color to match waybar
      "$riverctl" background-color 0x121212

      log "River config completed"
      tail "$logfile"
    '';
    };

    programs.waybar = {
      # waybar systemd integration is handled automatically by
      # programs.waybar.enable
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
            tag-labels = [ "edit" "2" "web" "4" "5" "6" "7" "8" "chat" "0" ];
          };

          "river/window" = {
            format = " {}";
            max-length = 50;
          };

          "river/mode" = {
            format = "{}";
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

    # Tie mako service to river-session.target:
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


    home.packages = with pkgs; ([
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
      swayimg
      imv

      pamixer

      userRiver
      (pkgs.writeShellScriptBin "system-swaylock" ''
        exec "$(command -v swaylock || echo /usr/bin/swaylock)" "$@"
      '')
      (pkgs.writeShellScriptBin "pick-foot" ''
        exec ${foot} --app-id=launcher --title=launcher \
             -e 'bash' '-c' \
                 'compgen -c |
                  grep -v fzf |
                  sort -u |
                  fzf --layout=reverse |
                  xargs -r ${riverctl} spawn'
    '')
      (pkgs.writeShellScriptBin "yazi-popup" ''
        exec ${foot} --app-id=yazi-popup --title=yazi-popup \
             -e ${pkgs.yazi}/bin/yazi
    '')

      pkgs.fuzzel

      # Ruby scripts (added to packages for availability in PATH)
      launcherScript
      wifiMenuScript
      systemMenuScript

      # Launcher commands using the factored helpers
      (mkLauncher {
        name = "pick-ruby";
        script = launcherScript;
      })

      (mkMenuSelector {
        name = "wifi-menu";
        script = wifiMenuScript;
        prompt = "WiFi: ";
        action = ''
          ${riverctl} spawn "${foot} -e sh -c '${iwctl} station wlan0 connect \"$selected\"'"
        '';
      })

      (mkMenuSelector {
        name = "system-menu";
        script = systemMenuScript;
        prompt = "System: ";
        action = ''
          ${lib.getExe rubyVersion} "${systemMenuScript}/bin/${systemMenuScript.name}" "$selected"
        '';
      })

      # Quick window/app switcher
      (pkgs.writeShellScriptBin "window-menu" ''
        ${riverctl} spawn "${fuzzelBin}" '')
      river-classic

      (pkgs.writeShellScriptBin "rebuild-river" ''
      log() {
            echo >&2 ">>>> $@"
      }

      if rebuild-home; then
        log "checking if river is running"
        # if river is running, reload its config
        if pgrep -x river >/dev/null; then
          ~/.config/river/init
          log 'reloaded river'
        else
          log 'river not running, skipping reload'
        fi
      else
        log "rebuild failed - not reloading river"
        exit 1
      fi
    '')
    ] ++ lib.optionals withNixGL [
      nixgl.nixGLMesa
      (lib.hiPrio riverPackage)  # nixGL-wrapped river takes precedence in PATH
    ]);


    home.sessionVariablesExtra = ''
      if [[ -z "$XDG_DATA_HOME" ]]; then
        export XDG_DATA_HOME="$HOME/.local/share"
      fi
    '';
  };
}
