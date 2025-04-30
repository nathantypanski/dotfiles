{
  description = "My full system config (Arch + Home Manager)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }: let
    system = "x86_64-linux"; # Change to "aarch64-linux" if you’re on ARM
    user = builtins.getEnv "USER";

    secrets = {
      userEmail = builtins.getEnv "USER_EMAIL";
    };
  in {
    # "System-level" configuration
    packages.${system}.default = nixpkgs.legacyPackages.${system}.hello;

    # Home-manager configuration
    homeConfigurations.${user} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
      };

      modules = [
        ../home-manager/arch.nix
        {
          home.username = user;
          home.homeDirectory = builtins.getEnv "HOME";

          programs.home-manager.enable = true;
          home.stateVersion = "24.11";
        }
      ];
      extraSpecialArgs = {
        inherit system;
        secrets = secrets;
        username = user;
        homeDirectory = "/home/${user}";
      };
    };
  };
}
