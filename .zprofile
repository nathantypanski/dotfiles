
#eval $(keychain --eval --agents ssh,gpg --clear id_rsa);
eval `keychain --eval --agents ssh,gpg ~/.ssh/id_rsa ~/.ssh/id_rsa_athen@ephesus`
TERM=xterm;
