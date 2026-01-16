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
      (treesit-grammars.with-grammars (grammars: with grammars; [
        tree-sitter-ruby
        tree-sitter-python
        tree-sitter-go
        tree-sitter-rust
        tree-sitter-javascript
        tree-sitter-typescript
        tree-sitter-yaml
        tree-sitter-bash
        tree-sitter-dockerfile
        tree-sitter-c
        tree-sitter-sql
      ]))
    ]);
  };

  home.packages = with pkgs; [
    # System dependencies for vterm
    (aspellWithDicts
      (dicts: with dicts; [ de en en-computers en-science es fr la ]))

    cmake
    pinentry-emacs
    (pkgs.writeShellScriptBin "bundle-rubocop" ''
      exec bundle exec rubocop "$@"
    '')
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
    "${lib.setPrio 1 ruby}"
    rubyPackages.ruby-lsp
    sshfs
    # for pdf rendering
    ghostscript
  ]);

  services.gpg-agent = {
    extraConfig = ''
      allow-emacs-pinentry
    '';
  };
}
