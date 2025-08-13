{ config, pkgs, lib, ... }:

let
  firefox-graphics = ''
      # this allows firefox to work under nixGL sway
      MESA_DRI_PATH=$(echo "''$LIBGL_DRIVERS_PATH" | cut -d':' -f1)
      if [[ -n "''$MESA_DRI_PATH" ]]; then
        # Convert /path/to/mesa/lib/dri to /path/to/mesa/lib/gbm
        export GBM_BACKENDS_PATH="''${MESA_DRI_PATH%/dri}/gbm"
        echo "Setting GBM_BACKENDS_PATH: ''$GBM_BACKENDS_PATH"
      fi

      export MOZ_ENABLE_WAYLAND=1
    '';
  firefox-jailed = (pkgs.writeShellScriptBin "firefox-jailed"
    (firefox-graphics + ''
      exec firejail ${lib.getExe pkgs.firefox} "$@"
    ''));
  firefox-devedition-jailed = (pkgs.writeShellScriptBin
    "firefox-devedition-jailed" (firefox-graphics + ''
      exec firejail ${lib.getExe pkgs.firefox-devedition} "$@"
    ''));
  tor-jailed = (pkgs.writeShellScriptBin "tor-jailed" (firefox-graphics + ''
      exec firejail ${lib.getExe pkgs.tor-browser} "$@"
    ''));
in
{
  options.ndt-home.firefox-jailed = lib.mkOption {
    type = lib.types.package;
    default = firefox-jailed;
    description = "Firefox wrapped with firejail";
  };

  config = {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox-devedition;
    };

    home.packages = with pkgs; [
      firefox-jailed
      firefox-devedition-jailed
      firefox-devedition
      mullvad-browser
      tor-jailed
      config.ndt-home.firefox-jailed
    ];
  };
}
