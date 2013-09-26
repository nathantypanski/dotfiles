#!/usr/bin/zsh

process () {
    if [[ ! -z "$1" ]]; then
        if top -b -n 1 | grep $1 >/dev/null; then
            return true
        else
            return false
        fi
    else
        return false
    fi
}
#if ! process conky ; then 
#    conky -c ~/.conkygraphs &
#fi
#if ! process termite ; then 
#    termite &
#fi
#if ! process firefox; then
#    firefox &
#fi
#if ! process compton; then 
#    compton &
#fi
#if ! process keychain; then 
#    eval `keychain --eval --agents ssh,gpg ~/.ssh/id_rsa ~/.ssh/id_rsa_athen@ephesus` &
#fi
#
#hsetroot -solid '#1d1f21' &
