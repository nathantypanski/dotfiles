#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

. "$SCRIPT_DIR/secrets.sh"

export NIXPKGS_ALLOW_UNFREE=1

# use latest nix for lazy-trees patches
nix shell nixpkgs#nixVersions.latest --command nix run \
    github:nix-community/home-manager \
    -- switch --impure --flake "${SCRIPT_DIR}"
