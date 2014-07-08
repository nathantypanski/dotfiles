
# Grep for a process name, excluding grep from the output.
function psgrep() {
    ps aux | grep $1 | grep -v grep
}

# Spawn a daemonized script ($1) only if it is not already started.
# If $2 is provided, this will run the daemon in that folder.
function daemonize_script () {
    if [[ -n $2 ]]; then
        pushd
        cd $2
    fi
    exists=$(psgrep $1)
    if [[ -z "$exists" ]]; then
        nohup $1
    fi
    if [[ -n $2 ]]; then
        popd
    fi
}

daemonize_script gitit ~/wiki > /dev/null 2>&1 &
