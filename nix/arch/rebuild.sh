#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

MANUAL_GCROOTS_DIR="$HOME/.local/state/nix/gcroots/manual/"

. "$SCRIPT_DIR/secrets.sh"

export NIXPKGS_ALLOW_UNFREE=1

# use latest nix for lazy-trees patches
# nix shell nixpkgs#nixVersions.latest --command nix run \
#     github:nix-community/home-manager \
#     -- switch --impure --flake "${SCRIPT_DIR}"

mkdir -p "${MANUAL_GCROOTS_DIR}"

(
  echo "$SCRIPT_DIR"
  # cd "${SCRIPT_DIR}" && nix shell nixpkgs#nixVersions.latest \
  cd "${SCRIPT_DIR}" && nix build "${@}" --impure "${SCRIPT_DIR}#home-manager-activation"
  # Need to sort out warning: ignoring the client-specified setting 'max-substitution-jobs', because it is a restricted setting and you are not a trusted user
  # cd "${SCRIPT_DIR}" && nix build --offline --impure --substitute --max-substitution-jobs 0 "${SCRIPT_DIR}#home-manager-activation"
)

ln -sf "$(readlink "${SCRIPT_DIR}/result")" "${MANUAL_GCROOTS_DIR}"

"${SCRIPT_DIR}/result/activate"
exit $?
