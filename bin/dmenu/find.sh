
#!/bin/bash
#input="$(xsel -o | $HOME/.bin/dmenu -p "Bestand zoeken:" )"
#if [ "$input" != '' ]; then
# result="$(echo "$input" | locate -e -r "$input" | $HOME/.bin/dmenu -p "Zoekresultaat:" )"
# xdg-open "$result"
#fi


find ~/ | sed 's/ /\\ /g' | sort -f | dmenu -i -l 20 -nb '#2e3436' -nf '#9999CC' | xargs exo-open

