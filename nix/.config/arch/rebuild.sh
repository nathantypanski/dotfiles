#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

. "$SCRIPT_DIR/secrets.sh"

export NIXPKGS_ALLOW_UNFREE=1

nix run github:nix-community/home-manager -- switch --impure --flake "${SCRIPT_DIR}"
