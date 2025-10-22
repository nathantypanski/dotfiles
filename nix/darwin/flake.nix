# ~/.config/nix/flake.nix

{
  description = "My system configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
        url = "github:LnL7/nix-darwin";
        inputs.nixpkgs.follows = "nixpkgs";
    };
    # Bleeding‑edge Emacs overlay with native‑comp by default
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";   # reuse your pinned nixpkgs
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, emacs-overlay }:
  let
    system = "aarch64-darwin";

    username = "ndt";
    secrets = {
      userEmail = builtins.getEnv "USER_EMAIL";
    };
    security.pam.enableSudoTouchIdAuth = true;
    homeDirectory = "/Users/${username}";
    configuration = {pkgs, ... }: {
      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";
      nix.settings.extra-substituters = [
        "https://nix-community.cachix.org"
      ];
      nix.settings.extra-trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      nix.settings.extra-platforms = ["x86_64-linux" "aarch64-linux"];
      nix.settings.extra-sandbox-paths = [ "/System/Library/Frameworks" "/System/Library/PrivateFrameworks" ];

      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility. please read the changelog
      # before changing: `darwin-rebuild changelog`.
      system.stateVersion = 4;

      # The platform the configuration will be used on.
      # If you're on an Intel system, replace with "x86_64-darwin"
      nixpkgs.hostPlatform = "aarch64-darwin";

      # Declare the user that will be running `nix-darwin`.
      users.users.ndt = {
        name = username;
        home = "/Users/${username}";
      };

      ids.gids.nixbld = 350;

      system.primaryUser = username;

      environment.systemPackages = with pkgs; [
        neovim
        git
        pass
        gopls
        grml-zsh-config
        (emacs.override { withNativeCompilation = true; })
      ];

      homebrew = {
        enable = true;
        # onActivation.cleanup = "uninstall";

        taps = [ ];
        brews = [ ];
        casks = [];
      };

    };
    nix.config = {
    };

  in
  {
    darwinConfigurations.H640WQ7FHV = nix-darwin.lib.darwinSystem {
      system = system;

      modules = [
        configuration
        home-manager.darwinModules.home-manager {
          nixpkgs = {
            overlays = [
              inputs.emacs-overlay.overlay
            ];
            config.allowUnfree = true;
          };

          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {
            inherit system secrets nixpkgs homeDirectory username;
          };
          # I almost wish I were using a standalone home-manager instead
          # of it being a darwin module, because it's annoying how modules
          # can't be imported here.
          #
          # If you want home modules, you have to import them in darwin.nix.
          home-manager.users.ndt = import ../home-manager/darwin.nix;
        }
      ];
    };
  };
}
