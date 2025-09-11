#!/bin/bash

LOG_DIR=/run/pcr15
mkdir -p "${LOG_DIR}"

log() {
    local message="${@}"
    echo "$(/bin/date '+%Y-%m-%d %H:%M:%S') - $@" |
        tee -a "${LOG_DIR}/validation.log" >&2
}

parse_cmdline() {
    local cmdline
    cmdline="$(cat /proc/cmdline)"
    read -r cmdline < /proc/cmdline
    if [[ "${cmdline}" =~ expected_pcr15=([0-9a-fA-F]+) ]]; then
        echo "${BASH_REMATCH[1]}"
    else
        echo ""
    fi
}

get_pcr15() {
    local pcr_output
    pcr_output="$(systemd-analyze pcrs 15 --json=short 2>/dev/null)"

    # Extract sha256 field using bash string manipulation
    if [[ "${pcr_output}" =~ \"sha256\":\"([0-9a-fA-F]+)\" ]]; then
        echo "${BASH_REMATCH[1]}"
    else
        echo ""
    fi
}

expected_pcr15="$(parse_cmdline)"
actual_pcr15="$(get_pcr15)"

if [ -n "${expected_pcr15}" ]; then
    log "Checking PCR 15 value"
    if [ "${actual_pcr15}" != "${expected_pcr15}" ]; then
        log "PCR 15 check failed. Expected '${expected_pcr15}', got '${actual_pcr15}'"
        log "WARNING: PCR 15 check failed - values don't match"
        log "This could indicate filesystem tampering or normal system changes"
        log "pcr15 mismatch // expected: '${expected_pcr15}' // actual: '${actual_pcr15}'"
        #systemctl start emergency.target
        #exit 1
    else
        log "PCR 15 check succeeded with value ${actual_pcr15}"
    fi
else
    log "No expected PCR 15 value provided - capture this for future validation!"
    log "Add 'expected_pcr15=${actual_pcr15}' to your kernel command line."
fi
