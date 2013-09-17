#!/bin/sh
cachedir=${XDG_CACHE_HOME:-"$HOME/.cache"}
if [ -d "$cachedir" ]; then
	cache=$cachedir/dmenu_run
else
	cache=$HOME/.dmenu_cache # if no xdg dir, fall back to dotfile in ~
fi
(
	IFS=:
	if stest -dqr -n "$cache" $PATH; then
		stest -flx $PATH | sort -u | tee "$cache" | ~/bin/dmenu/dmenu.sh "$@"
	else
		~/bin/dmenu/dmenu.sh -f "$@" < "$cache"
	fi
) | ${SHELL:-"/bin/sh"} &
