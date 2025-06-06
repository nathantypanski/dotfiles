{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    (writeShellScriptBin "git-clone-structured"
      (builtins.readFile ./git-clone-structured.sh))j
  ];
}
