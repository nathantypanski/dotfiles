{ lib, fetchFromGitHub, tree-sitter }:

# Not in nixpkgs' tree-sitter-grammars set, so we build it ourselves.
# Canonical upstream is codeberg.org/maribu/tree-sitter-proverif; GitHub is a mirror.
tree-sitter.buildGrammar {
  language = "proverif";
  version = "0.0.1"; # matches tree-sitter.json; rev below is 2026-07-27

  src = fetchFromGitHub {
    owner = "maribu";
    repo = "tree-sitter-proverif";
    rev = "7741807092c4009c1fe4c3648da60ca72b1b80f1";
    hash = "sha256-fMtGQsEbZHNIamrz3hoGvCJn5Dtu0oHhuLmMxOjFhGU=";
  };

  meta = {
    description = "ProVerif's typed pi calculus grammar for tree-sitter";
    homepage = "https://codeberg.org/maribu/tree-sitter-proverif";
    license = lib.licenses.bsd0;
  };
}
