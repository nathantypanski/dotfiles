{ config, pkgs, ... }:

let
  langs = [
    "python"
    "go"
    "rust"
    "javascript"
    "typescript"
    "yaml"
    "bash"
    "dockerfile"
    "c"
    "hcl"
    "sql"
  ];

  treeSitterFor = langs: p: map (lang: p.${"tree-sitter-" + lang}) langs;

  lspServers = builtins.filter (x: x != null) (map (lang:
    pkgs.${"${lang}-language-server"} or null
  ) langs);

in
{
  home.packages = with pkgs; [
    gopls
    rust-analyzer
    bash-language-server

    python313Packages.mcp
    python313Packages.python-lsp-server
    python313Packages.pylsp-mypy
    python313Packages.pylsp-rope
    python313Packages.python-lsp-ruff
    python313Packages.flake8

    (tree-sitter.withPlugins (treeSitterFor langs))
  ] ++ lspServers;


}
