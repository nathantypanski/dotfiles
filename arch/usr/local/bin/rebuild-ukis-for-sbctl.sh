#!/bin/bash
set -euo pipefail

LUKS_UUID="$(blkid -s UUID -o value /dev/nvme0n1p2)"
SWAP_UUID="$(blkid -s UUID -o value /dev/mapper/vg-swap)"
UKI_PATH_ROOT="/boot/efi/EFI/Manual"
DEFAULT_UKI_PATH="${UKI_PATH_ROOT}/arch.efi"
TROUBLESHOOT_UKI_PATH="${UKI_PATH_ROOT}/arch-troubleshoot.efi"
LTS_UKI_PATH="${UKI_PATH_ROOT}/arch-lts.efi"
TROUBLESHOOT_LTS_UKI_PATH="${UKI_PATH_ROOT}/arch-lts-troubleshoot.efi"
HARDENED_UKI_PATH="${UKI_PATH_ROOT}/arch-hardened.efi"
TROUBLESHOOT_HARDENED_UKI_PATH="${UKI_PATH_ROOT}/arch-hardened-troubleshoot.efi"

# usbcore quirks - fix Goodix Fingerprint USB Device
# nvme_core.default_ps_max_latency_us=
#
# per fw support, setting nvme.noacpi=1 and avoiding kernel arg for latency max
#
# 2025-08-10: nvme.noacpi=1 caused errors on resume
#
# namely, on the resume console:
#
#     nvme 0000:bf:00.0: Unable to change power state from D3cold to D0, device inaccessible
#
COMMON_CMDLINE="root=/dev/mapper/vg-main rw rootflags=subvol=@ usbcore.quirks=27c6:609c:0x40 rd.luks.name=${LUKS_UUID}=cryptroot rd.luks.options=${LUKS_UUID}=tpm2-device=auto,tpm2-measure-pcr=yes,discard,tries=3 lsm=capability,landlock,lockdown,yama,apparmor,bpf mitigations=full"
SECURE_CMDLINE="audit=1 iommu=pt"
SUSPEND_CMDLINE="rtc_cmos.use_acpi_alarm=1"
RESUME_CMDLINE="resume=UUID=${SWAP_UUID}"
TROUBLE_CMDLINE="log_level=7 rd.debug no_console_suspend"

SIGN_TOOL=sbsign
SECUREBOOT_PRIVATE_KEY="/var/lib/sbctl/keys/db/db.key"
SECUREBOOT_CERTIFICATE="/var/lib/sbctl/keys/db/db.pem"

PCR_PRIVATE_KEY='/var/lib/pcr-keys/pcr-initrd.key.pem'
PCR_PUBLIC_KEY='/var/lib/pcr-keys/pcr-initrd.pub.pem'

PCR15="98f31cd3ddfde2e5c8a1f6ce90a078bd0b6a3a66ca39133ec5b6a26b7e10925a" # '8361aaa7b1081fc3059a3c29471a7af284c42ee9afdc0be39236dc6803f4483d'

echo >&2 "cmdline:"
echo >&2 "  ${COMMON_CMDLINE}"

echo >&2 "hardened cmdline:"
echo >&2 "  ${COMMON_CMDLINE} ${SECURE_CMDLINE}"

declare -A ukis

build_uki() {
    local kernel_path="$1"
    local initrd_path="$2"
    local cmdline="$3"
    local output_path="$4"
    local pcr15_value="${5:-}"

    local new_uki_hash
    local base="$(basename ${output_path})"

    ukis["${output_path}"]="$(sha256sum "${output_path}")"

    echo >&2 "... [${base}] ..."
    # Add expected PCR 15 to kernel command line
    if [[ -n "${pcr15_value}" ]]; then
        echo >&2 "got pcr15 value: ${pcr15_value}"
        cmdline="${cmdline} expected_pcr15=${pcr15_value}"
    fi

    local cmdline_file="$(mktemp)"
    echo "$cmdline" > "${cmdline_file}"
    trap "rm -f '${cmdline_file}'; echo >&2 '... [deleted tempfile: "${cmdline_file}"] ...'" EXIT
        # --pcrs=0,6,7 \

    echo >&2 "... ["${base}" -> building] ..."
    set -x
    ukify build \
        --linux="${kernel_path}" \
        --initrd="${initrd_path}" \
        --microcode='/boot/amd-ucode.img' \
        --os-release="@/etc/os-release" \
        --cmdline="@${cmdline_file}" \
        --pcr-private-key="${PCR_PRIVATE_KEY}" \
        --pcr-public-key="${PCR_PUBLIC_KEY}" \
        --signtool="${SIGN_TOOL}" \
        --secureboot-private-key="${SECUREBOOT_PRIVATE_KEY}" \
        --secureboot-certificate="${SECUREBOOT_CERTIFICATE}" \
        --output="${output_path}" > /dev/null 2> /dev/null
    new_uki_hash-"$(sha256sum "${output_path}")"
    echo >&2 "... ["${base}" -> ${new_uki_hash} done!] ..."
    ukis["${output_path}"]="${new_uki_hash}"
    set +x
}

build_uki \
    "/boot/vmlinuz-linux" \
    "/boot/initramfs-linux.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${RESUME_CMDLINE} no_console_suspend rd.debug" \
    "${DEFAULT_UKI_PATH}" \
    "${PCR15}" &

build_uki \
    "/boot/vmlinuz-linux" \
    "/boot/initramfs-linux-fallback.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${TROUBLE_CMDLINE}" \
    "${TROUBLESHOOT_UKI_PATH}" \
    "${PCR15}" &

build_uki \
    "/boot/vmlinuz-linux-lts" \
    "/boot/initramfs-linux-lts.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${RESUME_CMDLINE} ${TROUBLE_CMDLINE} no_console_suspend rd.debug" \
    "${LTS_UKI_PATH}" \
    "${PCR15}" &

build_uki \
    "/boot/vmlinuz-linux-lts" \
    "/boot/initramfs-linux-lts-fallback.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${TROUBLE_CMDLINE}" \
    "${TROUBLESHOOT_LTS_UKI_PATH}" \
    "${PCR15}" &

build_uki \
    "/boot/vmlinuz-linux-hardened" \
    "/boot/initramfs-linux-hardened.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${SECURE_CMDLINE}" \
    "${HARDENED_UKI_PATH}" \
    "${PCR15}" &

build_uki \
    "/boot/vmlinuz-linux-hardened" \
    "/boot/initramfs-linux-hardened-fallback.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${TROUBLE_CMDLINE}" \
    "${TROUBLESHOOT_HARDENED_UKI_PATH}" \
    "${PCR15}" &

wait

echo >&2 "UKIs rebuilt and signed."
