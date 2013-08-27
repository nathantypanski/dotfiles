# better grml prompt
zstyle ':prompt:grml:*:percent' token '§ '
zstyle ':prompt:grml:*:percent' pre '%B%F{blue}'
zstyle ':prompt:grml:*:percent' post '%f%b'
zstyle ':prompt:grml:*:user' pre '%F{green}'
zstyle ':prompt:grml:*:user' post '%f'
zstyle ':prompt:grml:*:host' pre '%F{blue}'
zstyle ':prompt:grml:*:host' post '%f'
zstyle ':prompt:grml:*:at' token '@'
zstyle ':prompt:grml:*:at' pre '%f'
zstyle ':prompt:grml:*:at' post '%f'
zstyle ':prompt:grml:*:sad-smiley' token '%(?..×)'
zstyle ':prompt:grml:*:sad-smiley' pre '%B%F{red}'
zstyle ':prompt:grml:*:sad-smiley' post '%f%b'
zstyle ':prompt:grml:*:path' token '%40<..<%~%<< '
zstyle ':prompt:grml:*:path' pre '%F{yellow}%B'
zstyle ':prompt:grml:*:path' post '%f%b' 

# Change vcs_info formats for the grml prompt. The 2nd format sets up
# $vcs_info_msg_1_ to contain "zsh: repo-name" used to set our screen title.
# TODO: The included vcs_info() version still uses $VCS_INFO_message_N_.
#       That needs to be the use of $VCS_INFO_message_N_ needs to be changed
#       to $vcs_info_msg_N_ as soon as we use the included version.
# these are the same, just with a lot of colors:

    zstyle ':vcs_info:*' actionformats "${MAGENTA}${NO_COLOR}%s${MAGENTA}${YELLOW}/${MAGENTA}${GREEN}%b${YELLOW}|${RED}%a${MAGENTA}${NO_COLOR} " \
                                       "zsh: %r"
    zstyle ':vcs_info:*' formats       "${MAGENTA}${NO_COLOR}%s${MAGENTA}${YELLOW}/${MAGENTA}${GREEN}%b${MAGENTA}${NO_COLOR}%} " \
                                       "zsh: %r"
    zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat "%b${RED}:${YELLOW}%r"
