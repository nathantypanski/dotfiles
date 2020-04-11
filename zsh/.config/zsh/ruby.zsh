
RBENV_PATH="$HOME/.rbenv/bin"
which rbenv &> /dev/null
if [[ "$?" -ne "0" && -d $RBENV_PATH ]]; then
    export PATH=$PATH:$RBENV_PATH
    bval "$(rbenv init -)"
fi
