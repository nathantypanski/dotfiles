
# KEYFILE="${HOME}"'/.ssh/my_id_rsa'
# if [[ -f "${KEYFILE}" ]]; then
#     eval "$(/usr/bin/keychain -Q -q --nogui --eval --agents ssh ${KEYFILE})"
# fi

export SSH_AUTH_SOCK="/run/user/1000/gnupg/S.gpg-agent.ssh"
gpg-connect-agent updatestartuptty /bye >/dev/null
# expor tSSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
