mktar() { tar cvf "${1%%/}.tar" "${1%%/}/"; }
mktgz() { tar cvzf "${1%%/}.tar.gz" "${1%%/}/"; }
mktbz() { tar cvjf "${1%%/}.tar.bz2" "${1%%/}/"; }
# Maps a whole subnet with nmap
nnet () {
    if [[ -n $1 ]] ; then
        sudo nmap -sS -P0 $1
    fi
}

# File extract from somewhere on the Internet.
extract () {
   if [ -f $1 ] ; then
       case $1 in
	*.tar.bz2)	tar xvjf $1 && cd $(basename "$1" .tar.bz2) ;;
	*.tar.gz)	tar xvzf $1 && cd $(basename "$1" .tar.gz) ;;
	*.tar.xz)	tar Jxvf $1 && cd $(basename "$1" .tar.xz) ;;
	*.bz2)		bunzip2 $1 && cd $(basename "$1" /bz2) ;;
	*.rar)		unrar x $1 && cd $(basename "$1" .rar) ;;
	*.gz)		gunzip $1 && cd $(basename "$1" .gz) ;;
	*.tar)		tar xvf $1 && cd $(basename "$1" .tar) ;;
	*.tbz2)		tar xvjf $1 && cd $(basename "$1" .tbz2) ;;
	*.tgz)		tar xvzf $1 && cd $(basename "$1" .tgz) ;;
	*.zip)		unzip $1 && cd $(basename "$1" .zip) ;;
	*.Z)		uncompress $1 && cd $(basename "$1" .Z) ;;
	*.7z)		7z x $1 && cd $(basename "$1" .7z) ;;
	*)		echo "don't know how to extract '$1'..." ;;
       esac
   else
       echo "'$1' is not a valid file!"
   fi
}

absend () {
    if [ -f $1 ] ; then
        adb push $1 /sdcard/Books
    else
        echo "'$1' is not a valid file!"
    fi
}

absendall () {
    for file in `ls`
    do
        adb push $file /sdcard/Books
    done >& results.out
}

# colored man pages
man() {
    env LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	LESS_TERMCAP_md=$(printf "\e[1;31m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_se=$(printf "\e[0m") \
	LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[1;32m") \
	man "$@"
}

ranger-cd () {
    local WAS_TITLEABLE_TERM
    if [[ $IS_TITLEABLE_TERM -eq 1 ]]; then
        IS_TITLEABLE_TERM=0
        WAS_TITLEABLE_TERM=1
    fi
    tempfile='/tmp/chosendir'
    BUFFER='ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    test -f "$tempfile" &&
    if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
        cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"'
    zle accept-line
    NOPRECMD=0
    if [[ $WAS_TITLEABLE_TERM -eq 1 ]]; then
        IS_TITLEABLE_TERM=1
    fi
}

function open-urxvt-here () { urxvt &! }

# unregister broken GHC packages. Run this a few times to resolve dependency rot in installed packages.
# ghc-pkg-clean -f cabal/dev/packages*.conf also works.
function ghc-pkg-clean() {
for p in `ghc-pkg check $* 2>&1 | grep problems | awk '{print $6}' | sed -e 's/:$//'`
do
echo unregistering $p; ghc-pkg $* unregister $p
done
}
 
# remove all installed GHC/cabal packages, leaving ~/.cabal binaries and docs in place.
# When all else fails, use this to get out of dependency hell and start over.
function ghc-pkg-reset() {
read -p 'erasing all your user ghc and cabal packages - are you sure (y/n) ? ' ans
test x$ans == xy && ( \
echo 'erasing directories under ~/.ghc'; rm -rf `find ~/.ghc -maxdepth 1 -type d`; \
echo 'erasing ~/.cabal/lib'; rm -rf ~/.cabal/lib; \
# echo 'erasing ~/.cabal/packages'; rm -rf ~/.cabal/packages; \
# echo 'erasing ~/.cabal/share'; rm -rf ~/.cabal/share; \
)
}
 
alias cabalupgrades="cabal list --installed | egrep -iv '(synopsis|homepage|license)'"
