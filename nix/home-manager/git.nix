{ lib, pkgs, homeDirectory, username, userEmail, ... }:

{
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
          program = "/etc/profiles/per-user/${username}/bin/ssh-keygen";
        };
        core = {
          sshCommand = "/etc/profiles/per-user/${username}/bin/ssh";
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
