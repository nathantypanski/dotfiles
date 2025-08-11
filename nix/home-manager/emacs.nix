{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable-pgtk;
    extraPackages = epkgs: with epkgs; [
      vterm
    ];
  };

  home.packages = with pkgs; [
    # System dependencies for vterm
    libvterm
    cmake
    libtool
    
    pinentry-emacs
    (pkgs.writeShellScriptBin "rage-emacs" ''
      export PINENTRY_PROGRAM=${pkgs.pinentry-emacs}/bin/pinentry-emacs
      export PATH=${pkgs.age-plugin-yubikey}/bin:${pkgs.age-plugin-tpm}/bin:${pkgs.pinentry-emacs}/bin}:$PATH
      exec ${pkgs.rage}/bin/rage "$@"
    '')
  ];

  services.gpg-agent = {
    extraConfig = ''
      allow-emacs-pinentry
    '';
  };
}