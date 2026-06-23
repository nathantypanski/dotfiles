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
  fuzzelBin = "${lib.getExe pkgs.fuzzel}";
  term = "${lib.getExe pkgs.foot}";
  iwctl = "${pkgs.iwd}/bin/iwctl";

  # The default display scale is multiplied by $scaleRatio to get
  # final sizes for windows.
  scaleRatio = 2.0;
  scratchpadTag = "2147483648"; # 1 << 31

  pointer="pointer-2362-628-PIXA3854:00_093A:0274_Touchpad";

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
    menu_selector_script_path="${script}/bin/${script.name}"
    if [ -n "$selected" ]; then
      ${action}
    fi
  '';

  wgDownScript = pkgs.writeScriptBin "wg-down" ''
    set -euo pipefail
    interface="$(sudo wg show | awk 'NR==1 { print $2; }')"

    if ! [[ -z "$interface" ]]; then
      tput bold
      tput setaf 1
      echo -e >&2 "!!! interface $interface is currently connected !!!"
      echo -e >&2 "bringing it down"
      sudo wg-quick down "$interface"
      tput "sgr0"
    fi
  '';

  vpnConnectScript = pkgs.writeScriptBin "vpn-connect" ''
    set -euo pipefail
    filepath="$1"

    withFont() {
      tput $1
      ''${@:2}
      tput sgr0
    }

    withFont bold echo >&2 ">>> launching config: $filepath!"
    # try abspath first
    if [[ -f "$filepath" ]]; then
      withFont bold echo >&2 ">>> config exists: $filepath!"
    elif [[ -f "/etc/wireguard/$filepath" ]]; then
      filepath="/etc/wireguard/$1"
      withFont bold echo >&2 ">>> config exists: $filepath!"
    elif [[ -f "/etc/wireguard/$1.conf" ]]; then
      filepath="/etc/wireguard/$1.conf"
      withFont bold echo >&2 ">>> config exists: $filepath!"
    else
      withFont bold echo >&2 "the config file you specified does not exist!"
      exit 1
    fi
    echo >&2 "connecting to $filepath"
    ${lib.getExe wgDownScript}
    sudo wg-quick up "$filepath"
  '';


  mkTerminalPopup = { name, term, itemsCommand, command, app-id ? "popup" }:
  pkgs.writeShellScriptBin "${name}" ''
    ${term} --app-id="${app-id}" -- ${lib.getExe pkgs.bash} -c "${itemsCommand} | ${lib.getExe pkgs.fzf} | xargs ${lib.getExe command}"
  '';

  wireguard-picker = (mkTerminalPopup {
    inherit term;
    name =  "wg-picker";
    itemsCommand = "ls /etc/wireguard";
    command = vpnConnectScript;
    app-id = "popup";
  });

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
          font = "DepartureMono Nerd Font:size=8";
          dpi-aware = "yes";
          show-actions = "yes";
          terminal = "${term}";
          # fuzzy -> levenshtein fuzzy matching (instead of fzf))
          match-mode = "fuzzy";

          # Appearance
          match-counter= true;
          width = 80;
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
          width = 5;
          radius = 5;
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
        enable = false;
      };

      settings = {
        border-width = 2;
        set-cursor-warp = "on-focus-change";
        default-layout = "rivertile";

        declare-mode = ["normal" "tag" "layout"];

        rule-add = {
          "-app-id" = {
            quake = [ "position 50x50" "float"];
            popup = "float";
            "launcher" = "float";
            "term" = "ssd";
            "web" = "tags 4";
            "logs" = "tags ${scratchpadTag}";
            # Make Firefox use server-side decorations (i.e., via tiling WM)
            "'firefox*'" = ["ssd" "tags 4"];
          };
          "-title" = {
            "pavucontrol" = "float";
            "'*Firefox*'" = "ssd";
            "rebuild-river" = {
              "-app-id" = {
                "popup" = "tags $scratchpadTag";
              };
            };
          };

        };

        input = {
          "${pointer}" = {
            tap = "disabled";
            tap-button-map = "lrm";
            click-method = "clickfinger";
            scroll-method = "two-finger";
            scroll-factor = "0.3";
            accel-profile = "adaptive";
            pointer-accel = "0.1";
          };
        };

        spawn = [
          "systemctl --user start river-session.target"
        ];
      };

      extraConfig = ''
      logfile='/tmp/river-debug.log'
      layoutBin='${pkgs.river-classic}/bin/rivertile'
      scratchpadTag=$(( 1 << 31 ))

      log() {
            echo <&2 "$(date): " "$@" | tee -a "$logfile"
      }

      "${riverctl}" map-pointer normal Super BTN_LEFT move-view
      "${riverctl}" map-pointer normal Super BTN_RIGHT resize-view

      # New windows spawn on focused tags only (not all visible tags)
      "${riverctl}" spawn-tagmask "$(( ~$scratchpadTag ))"

      # Remap Caps Lock to Control
      "${riverctl}" keyboard-layout -options ctrl:nocaps us

      ps auxw | grep "$layoutBin" | grep -v grep \
          || nohup "$layoutBin" -view-padding 0 -outer-padding 0 &

      # Basic keybinds
      log "Setting up keybindings"
      "${riverctl}" map normal Super Return spawn "${term}"
      "${riverctl}" map normal Super Tab spawn river-last-tag
      "${riverctl}" map normal Super P spawn pick-ruby
      "${riverctl}" map normal Super+Shift W spawn wg-picker
      "${riverctl}" map normal Super N spawn ns-popup
      "${riverctl}" map normal Super W spawn wifi-menu
      "${riverctl}" map normal Super+Shift P spawn system-menu
      # "${riverctl}" map normal Super Tab spawn window-menu
      "${riverctl}" map normal Super Q close
      "${riverctl}" map normal Super+Shift E exit
      "${riverctl}" map normal Super+Shift Space toggle-float
      "${riverctl}" map normal Super F toggle-fullscreen
      "${riverctl}" map normal Super+Shift semicolon spawn system-swaylock
      "${riverctl}" map normal Super+Shift apostrophe spawn "${term} --app-id=popup --title=rebuild-river -e 'rebuild-river'"
      "${riverctl}" map normal Super Y spawn yazi-popup
      "${riverctl}" map normal Super+Shift R spawn "sh ~/.config/river/init"
      "${riverctl}" map normal Super+Shift S spawn "wlr-randr --output eDP-1 --scale ${toString scaleRatio}"

      # Scratchpad functionality (like Sway)
      "${riverctl}" rule-add -app-id 'scratch' csd
      "${riverctl}" map normal Super+Shift minus set-view-tags "$scratchpadTag"
      "${riverctl}" map normal Super minus toggle-focused-tags "$scratchpadTag"      # Brightness controls
      "${riverctl}" map normal None XF86MonBrightnessUp spawn "${lib.getExe pkgs.brightnessctl} s '+5%'"
      "${riverctl}" map normal None XF86MonBrightnessDown spawn "${lib.getExe pkgs.brightnessctl} s '5%-'"

      # Focus controls
      "${riverctl}" map normal Super J focus-view next
      "${riverctl}" map normal Super K focus-view previous
      "${riverctl}" map normal Super+Shift J swap next
      "${riverctl}" map normal Super+Shift K swap previous

      # Layout controls - for rivertile
      "${riverctl}" map normal Super H send-layout-cmd rivertile "main-ratio -0.05"
      "${riverctl}" map normal Super L send-layout-cmd rivertile "main-ratio +0.05"
      "${riverctl}" map normal Super I send-layout-cmd rivertile "main-count +1"
      "${riverctl}" map normal Super D send-layout-cmd rivertile "main-count -1"

      # Layout orientation
      "${riverctl}" map normal Super V send-layout-cmd rivertile "main-location left"
      "${riverctl}" map normal Super+Shift V send-layout-cmd rivertile "main-location right"
      "${riverctl}" map normal Super B send-layout-cmd rivertile "main-location top"
      "${riverctl}" map normal Super+Shift B send-layout-cmd rivertile "main-location bottom"

      # Switch layout generators
      "${riverctl}" map normal Super M spawn "pkill rivertile; ${riverPackage}/bin/rivertile -view-padding 2 -outer-padding 1 &"
      "${riverctl}" map normal Super+Shift M spawn "${riverctl} default-layout rivertile"

      # Tag operations - Super+0-9 for quick access
      for i in $(seq 0 9); do
            tags=$(( i == 0 ? (1 << 9) : (1 << (i - 1)) ))

            # Switch to tag (records history for Super+Tab last-tag toggle)
            "${riverctl}" map normal "Super" "$i" spawn "river-focus-tag $tags"

            # Join/leave a tag in the current view without switching away
            "${riverctl}" map normal "Super+Control" "$i" toggle-focused-tags "$tags"

            # Move focused window to tag
            "${riverctl}" map normal "Super+Shift" "$i" set-view-tags "$tags"
      done

      # Tag mode - for moving windows to tags
      "${riverctl}" map normal Super T enter-mode tag
      "${riverctl}" map tag None Escape enter-mode normal

      "${riverctl}" map normal Super space spawn "sh -c 'notify-send -t 1000 \"River\" \"Tag Mode: Press 0-9\"; ${riverctl} enter-mode tag'"
      # Tag mode bindings (0-9 for tag operations)
      for i in $(seq 0 9); do
            tags=$(( i == 0 ? (1 << 9) : (1 << (i - 1)) ))

            # In tag mode: numbers switch to tags
            "${riverctl}" map tag None "$i" spawn "river-focus-tag $tags"
            # In tag mode: Shift+numbers move window to tag
            "${riverctl}" map tag Shift "$i" set-view-tags "$tags"
      done

      # Layout mode - for layout operations
      "${riverctl}" map normal Super G enter-mode layout
      "${riverctl}" map layout None h send-layout-cmd rivertile "main-ratio -0.05"
      "${riverctl}" map layout None l send-layout-cmd rivertile "main-ratio +0.05"
      "${riverctl}" map layout None i send-layout-cmd rivertile "main-count +1"
      "${riverctl}" map layout None d send-layout-cmd rivertile "main-count -1"
      "${riverctl}" map layout None v send-layout-cmd rivertile "main-location left"
      "${riverctl}" map layout None V send-layout-cmd rivertile "main-location right"
      "${riverctl}" map layout None b send-layout-cmd rivertile "main-location top"
      "${riverctl}" map layout None B send-layout-cmd rivertile "main-location bottom"
      "${riverctl}" map layout None Escape enter-mode normal

      # Set repeat rate
      "${riverctl}" set-repeat 50 300

      # Cursor theme
      "${riverctl}" xcursor-theme ${config.gtk.cursorTheme.name or "Adwaita"} ${toString (config.gtk.cursorTheme.size or 24)}

      # Set background color to match waybar
      "${riverctl}" background-color 0x121212

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
        targets = [ "river-session.target" ];
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
            format = "󰍛 {percentage}%";
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
          font-size: 13px;
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
          border-right: 2px solid #3f3f3f;
          padding-right: 8px;
        }

        #battery.charging {
          color: #dfaf8f;
        }
        #battery.critical {
          color: #705050;
          background-color: #3f2f2f;
        }
        #battery.warning {
          color: #f0dfaf;
        }
        #network {
          margin: 0 5px 0 0;
          color: #8cd0d3;
        }
      '';
    };
    services.mako = {
      enable = false;
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
        ExecStart = "${pkgs.wlr-randr}/bin/wlr-randr --output eDP-1 --scale ${toString scaleRatio}";
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

    programs.yazi = {
      enableZshIntegration = true;
      settings = {
        opener.pdf = [
          { run = "${lib.getExe pkgs.zathura} \"$@\""; orphan = true; desc = "Zathura"; }
        ];
        open = {
          rules = [
            { mime = "application/pdf"; use = [ "pdf" ]; }
            { name = "*.{pdf,PDF}";    use = [ "pdf" ]; }
            { name = "*"; use = [ "open" "reveal" ]; }
          ];
        };
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

      # Switch the focused tags to $1, recording the previous tag so that
      # `river-last-tag` (Super+Tab) can jump back. river doesn't expose the
      # currently focused tags to scripts, so we track them ourselves: every
      # normal/tag-mode digit switch goes through this wrapper.
      (pkgs.writeShellScriptBin "river-focus-tag" ''
        set -euo pipefail
        state="''${XDG_RUNTIME_DIR:-/tmp}/river-tags"
        new="$1"
        cur="$(cat "$state.current" 2>/dev/null || echo 1)"
        if [ "$new" != "$cur" ]; then
          printf '%s' "$cur" > "$state.previous"
          printf '%s' "$new" > "$state.current"
        fi
        ${riverctl} set-focused-tags "$new"
      '')

      # Jump back to the previously focused tag (toggles between the current
      # and previous tag, like Super+Tab in many window managers).
      (pkgs.writeShellScriptBin "river-last-tag" ''
        set -euo pipefail
        state="''${XDG_RUNTIME_DIR:-/tmp}/river-tags"
        prev="$(cat "$state.previous" 2>/dev/null || echo 1)"
        cur="$(cat "$state.current" 2>/dev/null || echo 1)"
        printf '%s' "$cur" > "$state.previous"
        printf '%s' "$prev" > "$state.current"
        ${riverctl} set-focused-tags "$prev"
      '')

      (pkgs.writeShellScriptBin "spawn-river" ''
        exec ${term} --app-id=launcher --title=launcher \
             -e 'bash' '-c' \
                 'compgen -c |
                  grep -v fzf |
                  sort -u |
                  fzf --layout=reverse |
                  xargs -r ${riverctl} spawn'
    '')
      (pkgs.writeShellScriptBin "yazi-popup" ''
        exec ${term} --app-id=popup --title=yazi-popup \
             -e ${pkgs.yazi}/bin/yazi
    '')
      (pkgs.writeShellScriptBin "ns-popup" ''
        exec ${term} --app-id=popup --window-size-chars=120x50 \
             -- ${lib.getExe pkgs.bash} -c "${lib.getExe pkgs.nix-search-tv} print | ${lib.getExe pkgs.fzf} --preview \"${lib.getExe pkgs.nix-search-tv} preview {}\" --scheme history"
    '')

      pkgs.fuzzel

      # Ruby scripts (added to packages for availability in PATH)
      vpnConnectScript
      wireguard-picker

      # Launcher commands using the factored helpers
      (mkLauncher {
        name = "pick-ruby";
        script = pkgs.writeScriptBin "launcher.rb" ''
          ${builtins.readFile ./../../bin/launcher.rb}
       '';
      })

      (mkMenuSelector {
        name = "wifi-menu";
        script = pkgs.writeScriptBin "wifi-menu.rb" ''
          ${builtins.readFile ./../../bin/wifi-menu.rb}
        '';
        prompt = "WiFi: ";
        action = ''
          ${riverctl} spawn "${term} -e sh -c '${iwctl} station wlan0 connect \"$selected\"'"
        '';
       })

      (mkMenuSelector {
        name = "system-menu";
        script = pkgs.writeScriptBin "system-menu.rb" ''
          ${builtins.readFile ./../../bin/system-menu.rb}
        '';
        prompt = "System: ";
        action = ''
          ${lib.getExe rubyVersion} "$menu_selector_script_path" "$selected"
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

    home.activation = {
      startScratchTmux = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if ! ${pkgs.tmux}/bin/tmux has-session -t scratch 2>/dev/null; then
          ${pkgs.tmux}/bin/tmux new-session -d -s scratch
        fi
      '';
    };
  };
}
