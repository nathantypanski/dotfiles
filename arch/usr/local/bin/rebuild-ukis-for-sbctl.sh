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
COMMON_CMDLINE="root=/dev/mapper/vg-main rw rootflags=subvol=@ usbcore.quirks=27c6:609c:0x40 rd.luks.name=${LUKS_UUID}=cryptroot rd.luks.options=${LUKS_UUID}=tpm2-device=auto,tpm2-measure-pcr=yes,discard,tries=3 lsm=capability,landlock,lockdown,yama,apparmor,bpf mitigations=full nvme_core.default_ps_max_latency_us=25000"
SECURE_CMDLINE="audit=1 iommu=pt"
SUSPEND_CMDLINE="rtc_cmos.use_acpi_alarm=1"
RESUME_CMDLINE="resume=UUID=${SWAP_UUID}"
TROUBLE_CMDLINE="log_level=7 rd.debug no_console_suspend"

SIGN_TOOL=sbsign
SECUREBOOT_PRIVATE_KEY="/var/lib/sbctl/keys/db/db.key"
SECUREBOOT_CERTIFICATE="/var/lib/sbctl/keys/db/db.pem"

PCR_PRIVATE_KEY='/var/lib/pcr-keys/pcr-initrd.key.pem'
PCR_PUBLIC_KEY='/var/lib/pcr-keys/pcr-initrd.pub.pem'

echo "cmdline:"
echo "  ${COMMON_CMDLINE}"

echo "hardened cmdline:"
echo "  ${COMMON_CMDLINE} ${SECURE_CMDLINE}"

build_uki() {
    local kernel_path="$1"
    local initrd_path="$2"
    local cmdline="$3"
    local output_path="$4"
    local pcr15_value="${5:-}"

    set -x
    # Add expected PCR 15 to kernel command line
    if [[ -n "${pcr15_value}" ]]; then
        echo "got pcr15 value: ${pcr15_value}"
        cmdline="${cmdline} expected_pcr15=${pcr15_value}"
    fi

    local cmdline_file="$(mktemp)"
    echo "$cmdline" > "${cmdline_file}"
    trap "rm -f '${cmdline_file}'; echo >&2 'Cleaned up temp file: ${cmdline_file}'" EXIT
        # --pcrs=0,6,7 \

    echo "Building $(basename "$output_path")..."
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
        --output="${output_path}"
    set +x
}

build_uki \
    "/boot/vmlinuz-linux" \
    "/boot/initramfs-linux.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${RESUME_CMDLINE} no_console_suspend rd.debug" \
    "${DEFAULT_UKI_PATH}" \
    '98f31cd3ddfde2e5c8a1f6ce90a078bd0b6a3a66ca39133ec5b6a26b7e10925a'

build_uki \
    "/boot/vmlinuz-linux" \
    "/boot/initramfs-linux-fallback.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${TROUBLE_CMDLINE}" \
    "${TROUBLESHOOT_UKI_PATH}" \
    '98f31cd3ddfde2e5c8a1f6ce90a078bd0b6a3a66ca39133ec5b6a26b7e10925a'

build_uki \
    "/boot/vmlinuz-linux-lts" \
    "/boot/initramfs-linux-lts.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${RESUME_CMDLINE} ${TROUBLE_CMDLINE} no_console_suspend rd.debug" \
    "${LTS_UKI_PATH}" \
    '98f31cd3ddfde2e5c8a1f6ce90a078bd0b6a3a66ca39133ec5b6a26b7e10925a'

build_uki \
    "/boot/vmlinuz-linux-lts" \
    "/boot/initramfs-linux-lts-fallback.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${TROUBLE_CMDLINE}" \
    "${TROUBLESHOOT_LTS_UKI_PATH}" \
    '98f31cd3ddfde2e5c8a1f6ce90a078bd0b6a3a66ca39133ec5b6a26b7e10925a'

build_uki \
    "/boot/vmlinuz-linux-hardened" \
    "/boot/initramfs-linux-hardened.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${SECURE_CMDLINE}" \
    "${HARDENED_UKI_PATH}" \
    '98f31cd3ddfde2e5c8a1f6ce90a078bd0b6a3a66ca39133ec5b6a26b7e10925a'

build_uki \
    "/boot/vmlinuz-linux-hardened" \
    "/boot/initramfs-linux-hardened-fallback.img" \
    "${COMMON_CMDLINE} ${SUSPEND_CMDLINE} ${TROUBLE_CMDLINE}" \
    "${TROUBLESHOOT_HARDENED_UKI_PATH}" \
    '98f31cd3ddfde2e5c8a1f6ce90a078bd0b6a3a66ca39133ec5b6a26b7e10925a'

echo "UKIs rebuilt and signed."
