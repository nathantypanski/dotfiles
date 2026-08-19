{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    (writeShellScriptBin "clone" ''
      ${builtins.readFile ./../../bin/clone}
    '')
    (writeShellScriptBin "wt" ''
      ${builtins.readFile ./../../bin/wt}
    '')
  ];
}
