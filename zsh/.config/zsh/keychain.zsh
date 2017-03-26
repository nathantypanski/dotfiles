
KEYFILE="${HOME}"'/.ssh/my_id_rsa'
if [[ -f "${KEYFILE}" ]]; then
    eval "$(/usr/bin/keychain -Q -q --nogui --eval --agents ssh ${KEYFILE})"
fi
