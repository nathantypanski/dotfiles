#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

. "$SCRIPT_DIR/secrets.sh"

nix build ".#darwinConfigurations.${HOSTNAME}.system"

sudo ./result/sw/bin/darwin-rebuild activate

# Previously
# nix run nix-darwin -- switch --impure --flake "${SCRIPT_DIR}"
