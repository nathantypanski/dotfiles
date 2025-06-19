# Firejail profile for Claude Code
# Restrictive sandbox with explicit allowlists

include globals.local

# Disable development tools and interpreters by default
# include disable-devel.inc
# include disable-exec.inc
# include disable-interpreters.inc

# Network access (enabled for API calls)
# net none

# Sound not needed
nosound

# No access to webcam/microphone
novideo

# Filesystem restrictions
# Deny access to sensitive directories
blacklist /boot
blacklist /opt
blacklist /root
blacklist /srv
blacklist /sbin
blacklist /lib64
blacklist /lib32
blacklist /libx32
blacklist /dev/kmsg
blacklist /dev/port
blacklist /dev/mem
blacklist /dev/kmem
blacklist /proc/sys
blacklist /proc/sysrq-trigger
blacklist /proc/kcore
blacklist /etc/shadow
blacklist /etc/gshadow
blacklist /etc/sudoers

# Deny access to sensitive home directories
blacklist ${HOME}/.ssh
blacklist ${HOME}/.gnupg
# blacklist ${HOME}/.config
# blacklist ${HOME}/.local
# blacklist ${HOME}/.cache
blacklist ${HOME}/.*history
blacklist ${HOME}/.bash*
blacklist ${HOME}/.zsh*

# Minimal whitelist for testing
whitelist ${HOME}
whitelist /nix/store
whitelist /usr
whitelist /lib
whitelist /lib64
whitelist /bin
whitelist /etc

# Read-only access to essential system files
read-only /etc/hosts
read-only /etc/resolv.conf
read-only /etc/passwd
read-only /etc/group

# No new privileges
nonewprivs

# Disable access to /tmp for execution
noexec /tmp

# Private /tmp directory
private-tmp

# Memory protection (disabled for Node.js compatibility)
# memory-deny-write-execute

# Seccomp filter
seccomp

# Drop capabilities
caps.drop all

# Run as unprivileged user
noroot

# Disable X11 forwarding if not needed
# Comment out if you need GUI features
# disable-x11

# Limit process tree
rlimit-nproc 1000

# Limit file handles
rlimit-nofile 1024