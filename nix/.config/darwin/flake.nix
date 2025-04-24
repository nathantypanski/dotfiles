# ~/.config/nix/flake.nix

{
  description = "My system configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
        url = "github:LnL7/nix-darwin";
        inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }:
  let
    configuration = {pkgs, ... }: {
        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";

        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility. please read the changelog
        # before changing: `darwin-rebuild changelog`.
        system.stateVersion = 4;

        # The platform the configuration will be used on.
        # If you're on an Intel system, replace with "x86_64-darwin"
        nixpkgs.hostPlatform = "aarch64-darwin";

        # Declare the user that will be running `nix-darwin`.
        users.users.ndt = {
            name = "ndt";
            home = "/Users/ndt";
        };

        ids.gids.nixbld = 350;

        # Create /etc/zshrc that loads the nix-darwin environment.
        programs.zsh.enable = true;

        environment.systemPackages = [
          pkgs.neovim
          pkgs.git
          pkgs.pass
          pkgs.helix
          pkgs.openssh
        ];
    };
    secrets = {
      userEmail = builtins.getEnv "USER_EMAIL";
    };
  in
  {
    darwinConfigurations.H640WQ7FHV = nix-darwin.lib.darwinSystem {
      modules = [
         configuration
         home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              username = "ndt";
              homeDirectory = "/Users/ndt";
	      secrets = secrets;
            };
            home-manager.users.ndt = import ../home-manager/darwin.nix;

            # Optionally, use home-manager.extraSpecialArgs to pass
            # arguments to home.nix
         }
      ];
    };
  };
}
