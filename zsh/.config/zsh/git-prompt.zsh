# Adapted from <http://blog.joshdick.net/2012/12/30/my_git_prompt_for_zsh.html>
# which was in turn adapted from code found at
# <https://gist.github.com/1712320>.


# Modify the colors and symbols in these variables as desired.
GIT_PROMPT_SYMBOL=""
GIT_PROMPT_PREFIX="["
GIT_PROMPT_SUFFIX="%{$reset_color%}]"
GIT_PROMPT_AHEAD="%{$fg[red]%}+ANUM%{$reset_color%}"
GIT_PROMPT_BEHIND="%{$fg[cyan]%}-BNUM%{$reset_color%}"
GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}m%{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}_%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}m%{$reset_color%}"
GIT_PROMPT_STAGED="%{$fg_bold[green]%}s%{$reset_color%}"

which-branch () {
    (git symbolic-ref -q HEAD \
            || \
            git name-rev \
                --name-only \
                --no-undefined \
                --always \
                HEAD) \
        2> /dev/null
}

parse_git_state() {
    local GIT_STATE=""

    local NUM_AHEAD="$(git-ahead)"
    if [ "$NUM_AHEAD" -gt 0 ]; then
        GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
    fi

    local NUM_BEHIND="$(git-behind)"
    if [ "$NUM_BEHIND" -gt 0 ]; then
        GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
    fi

    local GIT_DIR="$(git-dir)"
    if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
        GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
    fi

    if [[ -n "$(git-untracked)" ]]; then
        GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
    fi

    if ! git-modified; then
        GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
    fi

    if ! git diff --cached --quiet 2> /dev/null; then
        GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
    fi

    if [[ -n $GIT_STATE ]]; then
        echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
    fi

}

# If inside a Git repository, print its branch and state
git_prompt_string() {
  local git_where="$(which-branch)"
  [ -n "$git_where" ] && \
      {
          echo -n "$GIT_PROMPT_SYMBOL"
          echo -n "$(parse_git_state)"
          echo -n "$GIT_PROMPT_PREFIX"
          echo -n "%{$fg_bold[yellow]%}"
          echo -n "${git_where#(refs/heads/|tags/)}"
          echo "$GIT_PROMPT_SUFFIX"
      }
}

# Set the right-hand prompt
RPS1='$(git_prompt_string)'
