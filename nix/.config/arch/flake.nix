{
  description = "My full system config (Arch + Home Manager)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }: let
    system = "x86_64-linux"; # Change to "aarch64-linux" if you’re on ARM
    user = builtins.getEnv "USER";
  in {
    # "System-level" configuration
    packages.${system}.default = nixpkgs.legacyPackages.${system}.hello;

    # Home-manager configuration
    homeConfigurations.${user} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
      };

      home.username = user;
      home.homeDirectory = builtins.getEnv "HOME";

      programs.home-manager.enable = true;
      home.stateVersion = "24.11";

      imports = [
        ../home-manager/home.nix
      ];
    };
  };
}
