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
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay, nixgl, ... }: let
    system = "x86_64-linux";
    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";

    secrets = {
      userEmail = builtins.getEnv "USER_EMAIL";
    };
    hmConfig = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          emacs-overlay.overlay
          nixgl.overlay
        ];

      };

      modules = [
        {
          nixGL.packages = nixgl.packages;
          nixGL.defaultWrapper = "mesa";
        }
        ../home-manager/arch.nix
        {
          programs.home-manager.enable = true;
          home.stateVersion = "24.11";

          home.username = username;
          home.homeDirectory = homeDirectory;
        }
      ];
      extraSpecialArgs = {
        inherit system;
        secrets = secrets;
        username = username;
        homeDirectory = homeDirectory;
      };
    };
  in {
    homeConfigurations.${username} = hmConfig;
    packages.${system}.home-manager-activation = hmConfig.activationPackage;
  };
}
