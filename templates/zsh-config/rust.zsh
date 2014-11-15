# Emit proper debug symbols for rust debugging
# From http://stackoverflow.com/questions/24664994/debugging-rust-with-gdb

alias rd='rustc -g --emit="obj,link"'

compile_and_run() {
     rustc -g --emit="obj,link" $1 && gdb ${1%.*}
}

alias rdr=compile_and_run
