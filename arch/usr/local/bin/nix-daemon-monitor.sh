#!/bin/bash
while true; do
  count=$(journalctl -b --since="5 minutes ago" | grep "accepted connection" | wc -l)
  if [ $count -gt 50 ]; then
    echo "$(date): WARNING: $count nix-daemon connections in last 5 minutes" >> /var/log/nix-spam.log
    ps aux | grep nix >> /var/log/nix-spam.log
    journalctl -b --since="5 minutes ago" | grep "accepted connection" | tail -20 >> /var/log/nix-spam.log
    echo "---" >> /var/log/nix-spam.log
  fi
  sleep 300  # Check every 5 minutes
done
