OSX_27_VIRTUALENVWRAPPER="${HOME}/Library/Python/2.7/bin/virtualenvwrapper.sh"
OSX_3_VIRTUALENVWRAPPER="${HOME}/Library/Python/3.5/bin/virtualenvwrapper.sh"
LINUX_VIRTUALENVWRAPPER="${HOME}/.local/bin/virtualenvwrapper.sh"
if [[ `uname` = 'Darwin' && -f "${OSX_27_VIRTUALENVWRAPPER}" ]]; then
    source "${OSX_27_VIRTUALENVWRAPPER}" 
elif [[ "$(uname)" = 'Darwin'  && -f "${OSX_3_VIRTUALENVWRAPPER}" ]]; then
    export VIRTUALENVWRAPPER_PYTHON='/usr/local/bin/python3'
    source "${OSX_3_VIRTUALENVWRAPPER}" 
elif [[ "$(uname)" = 'Linux' && -f "${LINUX_VIRTUALENVWRAPPER}" ]]; then
    source "${LINUX_VIRTUALENVWRAPPER}"
fi
