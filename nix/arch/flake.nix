{
  description = "My full system config (Arch + Home Manager)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Bleeding‑edge Emacs overlay with native‑comp by default
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";   # reuse your pinned nixpkgs
    };

    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-latest.url = "github:NixOS/nix/master";
    nix-latest.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay, nixgl, ... }: let
    system = "x86_64-linux";
    lib = nixpkgs.lib;

    # Type-safe environment variable handling with validation
    username =
      let raw = builtins.getEnv "USER";
      in assert lib.assertMsg (raw != "") "USER environment variable must be set";
         raw;

    # Type-safe email validation with assertion
    userEmail =
      let raw = builtins.getEnv "USER_EMAIL";
      in assert lib.assertMsg (raw != "") "USER_EMAIL environment variable must be set";
         raw;
    # Get home directory from system passwd database if HOME unset
    getSystemHomeDir = username:
      let
        passwdContent = builtins.readFile /etc/passwd;
        lines = lib.splitString "\n" passwdContent;
        userLine = lib.findFirst
          (line: lib.hasPrefix "${username}:" line)
          null
          lines;
      in
        if userLine != null then
          let fields = lib.splitString ":" userLine;
          in lib.elemAt fields 5  # Home directory is 6th field (index 5)
        else
          throw "User ${username} not found in /etc/passwd";

    homeDirectory = validatePath (
      let raw = builtins.getEnv "HOME";
      in if raw != "" then raw
         else getSystemHomeDir username
    );

    # Validate paths exist and are absolute
    validatePath = path:
      assert lib.assertMsg (lib.hasPrefix "/" path) "Path must be absolute: ${path}";
      path;

    secrets = {
      userEmail = userEmail;
    };
    customOverlay = final: prev: {
      # Custom package modifications
      ungoogled-chromium-wayland = prev.ungoogled-chromium.override {
        commandLineArgs = [
          "--enable-features=UseOzonePlatform"
          "--ozone-platform-hint=auto"
          "--use-gl=egl"
          "--gtk-version=4"
          "--enable-features=WaylandPerSurfaceScale,WaylandUiScale"
        ];
      };

      signal-wayland = prev.writeShellScriptBin "signal" ''
        ${prev.signal-desktop}/bin/signal-desktop \
            --enable-features=UseOzonePlatform \
            --ozone-platform=wayland "$@"
      '';
    };

    hmConfig = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          emacs-overlay.overlay
          nixgl.overlay
          customOverlay
        ];
      };

      modules = [
        ../home-manager/arch.nix
        {
          # Add warnings for environment variables (only when actually building)
          warnings = lib.optionals (username != "" && homeDirectory != "") [
            (lib.optionalString (secrets.userEmail == "")
              "USER_EMAIL environment variable not set - git commits may not work properly")
          ];
        }
        {
          programs.home-manager.enable = true;
          home.stateVersion = "24.11";
          home.username = username;
          home.homeDirectory = homeDirectory;
        }
      ];
      extraSpecialArgs = {
        inherit system nixgl;
        secrets = secrets;
        username = username;
        homeDirectory = homeDirectory;
        home-manager.backupFileExtension = "bk";
      };
    };
  in {
    homeConfigurations.${username} = hmConfig;
    packages.${system}.home-manager-activation = hmConfig.activationPackage;
  };
}
