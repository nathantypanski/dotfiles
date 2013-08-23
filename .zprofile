# start ssh-agent
eval $(keychain --eval --agents ssh,gpg --clear id_rsa);
TERM=xterm;
