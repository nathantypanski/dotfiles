#!/usr/bin/env bash

set -euo pipefail
set -x

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

. "$SCRIPT_DIR/secrets.sh"

## Previously
#
# nix run nix-darwin -- switch --impure --flake "${SCRIPT_DIR}"
#
echo >&2 "running 'nix build .#darwinConfigurations.${HOSTNAME}.system'"

nix build ".#darwinConfigurations.${HOSTNAME}.system" --impure

echo >&2 "${SCRIPT_DIR}/result/sw/bin/darwin-rebuild activate"

sudo "${SCRIPT_DIR}/result/sw/bin/darwin-rebuild" "activate"
