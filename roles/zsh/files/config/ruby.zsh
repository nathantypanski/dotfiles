
RBENV_PATH="$HOME/.rbenv/bin"
which rbenv &> /dev/null
if [[ "$?" -eq "1" && -e $RBENV_PATH ]]; then
    export PATH=$PATH:$RBENV_PATH
fi
eval "$(rbenv init -)"
