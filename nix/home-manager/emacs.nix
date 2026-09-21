{ config, pkgs, ... }:

let
  emacsPackage = if pkgs.stdenv.isDarwin
              then pkgs.emacs-macport
              else pkgs.emacs-unstable-pgtk;

  # Not in nixpkgs' tree-sitter-grammars set; see nix/pkgs/.
  tree-sitter-proverif = pkgs.callPackage ../pkgs/tree-sitter-proverif.nix { };
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
      ] ++ [ tree-sitter-proverif ]))
    ]);
  };

  home.packages = with pkgs; [
    # System dependencies for vterm
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
