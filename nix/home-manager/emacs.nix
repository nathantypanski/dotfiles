{ config, pkgs, ... }:

let
  emacsPackage = if pkgs.stdenv.isDarwin
              then pkgs.emacs-macport
              else pkgs.emacs-unstable-pgtk;
in
{
  programs.emacs = {
    enable = true;
    package = emacsPackage;
    extraPackages = (epkgs: with epkgs; [
      vterm
    ]);
  };

  home.packages = with pkgs; [
    # System dependencies for vterm
    (aspellWithDicts
      (dicts: with dicts; [ de en en-computers en-science es fr la ]))

    pinentry-emacs
    (pkgs.writeShellScriptBin "rage-emacs" ''
      export PINENTRY_PROGRAM=${pkgs.pinentry-emacs}/bin/pinentry-emacs
      export PATH=${pkgs.age-plugin-yubikey}/bin:${pkgs.age-plugin-tpm}/bin:${pkgs.pinentry-emacs}/bin}:$PATH
      exec ${pkgs.rage}/bin/rage "$@"
    '')
  ] ++ (if stdenv.isDarwin then [
    # macOS-specific packages
    cmake
    libtool
  ] else [
    # Linux-specific packages
    libvterm
    ruby
    rubyPackages.sorbet-runtime
    rubyPackages.ruby-lsp
    rubyPackages.psych
    libyaml
  ]);

  services.gpg-agent = {
    extraConfig = ''
      allow-emacs-pinentry
    '';
  };
}
