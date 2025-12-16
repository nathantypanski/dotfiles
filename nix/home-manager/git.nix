{ homeDirectory, username, userEmail, secrets, ... }:

{
  programs.git = {
    enable = true;
    userName = username;
    userEmail = userEmail;
    # home-manager only supports gnupg, so try to workaround
    signing = {
      signByDefault = true;
    };
    aliases = {
      co = "checkout";
      br = "branch";
      ci = "commit";
      S  = "status";
      s  = "status --short";
    };
    extraConfig = {
      gpg.format = "ssh";
      gpg.ssh.allowedSignersFile = "${homeDirectory}/.config/git/allowed_signers";
      user.signingkey = secrets.sshSigningKey;
      # Use Nix OpenSSH with FIDO2 support for YubiKey (for both git operations and signing)
      gpg.ssh.program = "/etc/profiles/per-user/${username}/bin/ssh";
      core.sshCommand = "/etc/profiles/per-user/${username}/bin/ssh";
      url."git@github.com:".insteadOf = "https://github.com/";
    };
  };
}
