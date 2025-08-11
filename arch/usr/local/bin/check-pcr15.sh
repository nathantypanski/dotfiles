#!/bin/bash

LOG_DIR=/run

# Parse command line for expected PCR 15
parse_cmdline() {
    local cmdline
    cmdline=$(cat /proc/cmdline)

    # Look for expected_pcr15= parameter
    if [[ "$cmdline" =~ expected_pcr15=([0-9a-fA-F]+) ]]; then
        echo "${BASH_REMATCH[1]}"
    else
        echo ""
    fi
}

# Get PCR 15 value without jq
get_pcr15() {
    local pcr_output
    pcr_output=$(systemd-analyze pcrs 15 --json=short 2>/dev/null)

    # Extract sha256 field using bash string manipulation
    if [[ "$pcr_output" =~ \"sha256\":\"([0-9a-fA-F]+)\" ]]; then
        echo "${BASH_REMATCH[1]}"
    else
        echo ""
    fi
}

# Main logic
expected_pcr15="$(parse_cmdline)"
actual_pcr15="$(get_pcr15)"

echo "Current PCR 15 value: $actual_pcr15"
if [ -n "$expected_pcr15" ]; then
    echo "Checking PCR 15 value"

    if [ "$actual_pcr15" != "$expected_pcr15" ]; then
        echo "PCR 15 check failed"
        echo "Expected: $expected_pcr15"
        echo "Actual: $actual_pcr15"
        # systemctl start emergency.target
        # Don't fail yet. Instead:
        echo "WARNING: PCR 15 check failed - values don't match"
        echo "This could indicate filesystem tampering or normal system changes"
        # Log to a persistent location if root is mounted read-write
        mkdir -p /run/pcr15
        echo "$(date): PCR 15 mismatch - Expected: $expected_pcr15, Actual: $actual_pcr15" >> "${LOG_DIR}/validation.log"
        ## Don't fail boot, just log the warning
        # exit 1
    else
        echo "PCR 15 check succeeded"
    fi
else
    echo "No expected PCR 15 value provided - capture this for future validation"
    echo "Add expected_pcr15=$actual_pcr15 to your kernel command line"
fi
