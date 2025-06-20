# Simple Claude Code firejail profile for testing

# Allow network access for API calls
# net none

# Basic security
nosound
novideo
nonewprivs
noroot
private-tmp

# Block sensitive files and directories
blacklist ${HOME}/.ssh
blacklist ${HOME}/.gnupg
blacklist ${HOME}/.age
blacklist ${HOME}/.passage
blacklist ${HOME}/.password-store
blacklist ${HOME}/.mozilla
blacklist ${HOME}/.tor
blacklist ${HOME}/.electrum
blacklist ${HOME}/.*history
blacklist ${HOME}/.bash*
blacklist ${HOME}/.zsh*

# Cryptocurrency & Financial
blacklist ${HOME}/.bitcoin
blacklist ${HOME}/.ethereum
blacklist ${HOME}/.monero
blacklist ${HOME}/.wallet

# Development Secrets
blacklist ${HOME}/.aws
blacklist ${HOME}/.gcp
blacklist ${HOME}/.azure
blacklist ${HOME}/.docker/config.json
blacklist ${HOME}/.kube
blacklist ${HOME}/.terraform.d

# VPN/Network Configs
blacklist ${HOME}/.openvpn
blacklist ${HOME}/.wireguard
blacklist ${HOME}/.nordvpn

# Communication/Personal
blacklist ${HOME}/.thunderbird
blacklist ${HOME}/.signal
blacklist ${HOME}/.telegram
blacklist ${HOME}/.slack
blacklist ${HOME}/Documents/Personal
blacklist ${HOME}/Documents/Financial

# Cloud Sync
blacklist ${HOME}/Dropbox
blacklist ${HOME}/Google Drive
blacklist ${HOME}/.dropbox

# System/Admin
blacklist ${HOME}/.sudo_as_admin_successful
blacklist /etc/shadow
blacklist /etc/gshadow
blacklist /etc/sudoers.d

# Secrets Managers
blacklist ${HOME}/.1password
blacklist ${HOME}/.op
blacklist ${HOME}/.bitwarden
blacklist ${HOME}/.lastpass
blacklist ${HOME}/.dashlane
blacklist ${HOME}/.keeper
blacklist ${HOME}/.enpass
blacklist ${HOME}/.hashicorp-vault
blacklist ${HOME}/.vault
blacklist ${HOME}/.config/1Password
blacklist ${HOME}/.config/Bitwarden
blacklist ${HOME}/.config/op
blacklist ${HOME}/.local/share/1password
blacklist ${HOME}/.local/share/bitwarden

# Don't restrict /nix - needed for Nix to work properly
noblacklist /nix

# Allow specific work directories only (blocks everything else in homedir)
whitelist ${HOME}/src
whitelist ${HOME}/.claude
whitelist ${HOME}/.claude.json
whitelist ${HOME}/.config/claude
whitelist ${HOME}/.local/share/claude
whitelist ${HOME}/.cache/claude
whitelist ${HOME}/.nix-profile

# Allow /nix directory access without explicit whitelist

# Allow common system access
include whitelist-common.inc