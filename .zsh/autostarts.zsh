# Grep for a process name, excluding grep from the output.
function psgrep() {
    ps aux | grep $1 | grep -v grep
}

# Spawn a daemonized script ($1) only if it is not already started.
# If $2 is provided, this will run the daemon in that folder.
function daemonize_script () {
    if [[ -z $(psgrep $1) ]]; then
        if [[ -n $2 ]]; then
            olddir=$(pwd)
            cd $2
        fi
        nohup $1 > /dev/null 2>&1 &
        if [[ -n $2 ]]; then
            cd $olddir
        fi
    fi
}

daemonize_script ~/bin/start_gitit ~/.gitit
