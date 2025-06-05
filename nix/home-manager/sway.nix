{ config, pkgs, lib, mod, termFont, homeDirectory, withNixGL, ... }:


let
  swayWithNixGL = pkgs.symlinkJoin {
    name = "sway-with-nixgl";
    paths = [ pkgs.sway ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
        rm $out/bin/sway
            makeWrapper ${lib.getExe pkgs.nixgl.nixGLMesa} $out/bin/sway \
                  --add-flags ${pkgs.sway}/bin/sway
                    '';
  };
in
{
  home.pointerCursor = {
    package = pkgs.adwaita-icon-theme;
    name  = "Adwaita";   # any cursor theme in your icon path
    size  = 7;           # logical pixels → doubles on a 2×-scaled output
    gtk.enable  = true;  # write GTK settings files
    x11.enable  = true;  # export XCURSOR_* for XWayland & Flatpaks
    sway.enable = true;  # adds `seat * xcursor_theme …` to sway.conf
  };

  programs.i3status = {
    enable = true;
  };

  wayland.windowManager.sway = {
    # Only use nixGL on non-nixos systems.
    package = if withNixGL
              then swayWithNixGL
              else pkgs.sway;
    enable = true;
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
          pointer_accel = "-0.1";
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
        names = ["Terminus"];
        style = "normal";
        size = 9.0;
      };
      bars = [{
        statusCommand = "i3status";
        # statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${homeDirectory}/.config/i3status-rust/config-default.toml";
        fonts = {
          names = ["Terminus"];
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
          "${mod}+Shift+Return" = "exec --no-startup-id ${pkgs.foot}/bin/foot";

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
          "${mod}+Shift+apostrophe" = "exec foot --font '${termFont}:size=9' --window-size-chars=100x50 --app-id=popup-term -- bash -i -c \"rebuild-home; read -n 1 -s -r -p '[ press any key to continue ]'\"";
          "${mod}+Shift+r" = "exec ${pkgs.sway}/bin/swaymsg reload";
          "--release Print" = "exec --no-startup-id ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area";
          # nix swaylock is broken
          "${mod}+Shift+semicolon" = "exec ${pkgs.sway}/bin/swaylock -f -c 000000";
          "${mod}+p" = "exec --no-startup-id pick-foot";
          "${mod}+Shift+q" = "exec ${pkgs.sway}/bin/swaynag -t warning -m 'Exit Sway?' -b 'Yes' '${pkgs.sway}/bin/swaymsg exit'";
          "XF86MonBrightnessUp" = "exec --no-startup-id ${pkgs.brightnessctl}/bin/brightnessctl s 10+";
          "XF86MonBrightnessDown" = "exec --no-startup-id ${pkgs.brightnessctl}/bin/brightnessctl s 10-";
        }
      ];
      startup = [
        { command = "${pkgs.sway}/bin/swaymsg workspace 1"; always = true; }
        { command = "swaybg -c #2b2b2b"; always = true; }
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
      workspace_auto_back_and_forth yes
      default_border pixel 1
      default_floating_border pixel 3
    '';
    extraSessionCommands = ''
      # force software rendering - broken hardware rendering on arch
      # export WLR_RENDERER=pixman
      # export LIBGL_DRIVERS_PATH=/usr/lib/dri
      # export GBM_DRIVERS_PATH=/usr/lib/dri
    '';
  };
}
