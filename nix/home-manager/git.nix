{ lib, pkgs, homeDirectory, username, userEmail, ... }:

let
  # OpenSSH with FIDO2/U2F support for YubiKey security keys.
  # Pinned to the store path so ssh-keygen/ssh are guaranteed to exist
  # regardless of profile layout (standalone home-manager vs. NixOS).
  opensshFido = pkgs.openssh.override { withFIDO = true; };
in
{
  home.packages = [ opensshFido ];

  programs = {
    git = {
      enable = true;
      # home-manager only supports gnupg, so try to workaround
      signing = {
        # signer = "ssh-keygen";
        signByDefault = true;
        key = "${homeDirectory}/.ssh/id_ed25519.pub";
        format = "ssh";
      };
      settings = {
        user = {
          name = username;
          email = userEmail;
        };
        gpg.ssh = {
          allowedSignersFile = "${homeDirectory}/.config/git/allowed_signers";
          # Use Nix OpenSSH with FIDO2 support for YubiKey
          program = lib.getExe' opensshFido "ssh-keygen";
        };
        core = {
          sshCommand = lib.getExe' opensshFido "ssh";
          fsmonitor = false;
        };
        url."git@github.com:".insteadOf = "https://github.com/";
        aliases = {
          co = "checkout";
          br = "branch";
          ci = "commit";
          S  = "status";
          s  = "status --short";
          gpf = "push origin --force-with-lease";
          w  = "worktree";
          wt  = "worktree";
        };

        pager = {
          diff = "${lib.getExe pkgs.diffnav}";
        };
      };
    };
  };
}
