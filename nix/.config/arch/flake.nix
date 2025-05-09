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
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay, ... }: let
    system = "x86_64-linux";
    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";

    secrets = {
      userEmail = builtins.getEnv "USER_EMAIL";
    };
  in {
    homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ emacs-overlay.overlay ];
      };

      modules = [
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
  };
}
