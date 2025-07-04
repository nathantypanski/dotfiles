#!/usr/bin/env bash

simplecat() {
	local args="$@"
	for f in $args; do
		echo "$ cat "$f""
		cat "$f"
		echo
	done
}

simplecmd() {
	echo "$ $@"
	bash -c "$@"
	echo
}

simplecat /etc/os-release /etc/kernel/cmdline

simplecat $(ls /boot/efi/loader/entries/*)
simplecat /boot/efi/loader/loader.conf

for f in /etc/pacman.d/hooks/*; do
	echo "$ cat "${f}""
	cat "$f"
	echo

	ref="$(cat $f | grep '^Exec' | awk '{print $3;} ')"
	echo "$ cat "${ref}""
	cat "$ref"
	echo
done

echo

for cmd in status list-files verify sign-all; do
	echo "$ sbctl ${cmd}"
	sbctl "${cmd}"
	echo
done

echo "$ cryptsetup luksDump /dev/nvme0n1p2 | grep pcr"
cryptsetup luksDump /dev/nvme0n1p2 | grep pcr
echo

echo "$ bootctl status"
bootctl status | cat
echo

echo "$ lsblk"
lsblk
echo

echo "$ efibootmgr"
efibootmgr
echo

echo "$ tree /boot"
tree /boot
echo

simplecmd "systemd-analyze pcrs | cat"

simplecmd "dmesg | grep -i tpm"

simplecmd "lsmod | grep -i tpm"

simplecmd "ls -l /dev/tpm*"
