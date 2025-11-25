#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

. "$SCRIPT_DIR/secrets.sh"

export NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1

# prewarm sudo

sudo true

## Previously
#
# nix run nix-darwin -- switch --impure --flake "${SCRIPT_DIR}"
#
echo >&2 "running 'nix build .#darwinConfigurations.${HOSTNAME}.system'"

HOST=$(hostname) nix --extra-experimental-features flakes --extra-experimental-features nix-command build "${SCRIPT_DIR}#darwinConfigurations.default.system" --impure

echo >&2 "${SCRIPT_DIR}/result/sw/bin/darwin-rebuild activate"

sudo "${SCRIPT_DIR}/result/sw/bin/darwin-rebuild" "activate"
