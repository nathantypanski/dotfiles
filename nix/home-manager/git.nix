{ homeDirectory, username, userEmail, ... }:

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
      user.signingkey = "${homeDirectory}/.ssh/id_ed25519.pub";
      url."git@github.com:".insteadOf = "https://github.com/";
    };
  };
}
