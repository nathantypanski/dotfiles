{ config, pkgs, copyCommand, ... }:

{
  programs.tmux = {
    enable = true;
    extraConfig = ''
# config from nix
# Reasonable key prefix
unbind C-b
set-option -g prefix C-a
set-option -g status-keys vi # Use vi-style key bindings in the status line

# Unbind the default split commands
unbind %
unbind -
# New pane in the current directory
bind \\ split-window -h -c "#{pane_current_path}"
# New pane in the home directory
bind | split-window -h
bind - split-window -v -c "#{pane_current_path}"
bind _ split-window -v
bind-key x kill-pane

set -g mouse

# Give us a slightly higher repeat delay for repeatable keystrokes
#set repeat-time 750

# Allows us to use C-a a <command> to send commands to a TMUX session inside
# another TMUX session
bind-key s send-prefix

# I'm a Vim user, this makes navigation easier
setw -g mode-keys vi # I especially like being able to search with /,? when in copy-mode
unbind [


bind Escape copy-mode #-vi
unbind p
bind p paste-buffer
bind-key p run-shell "tmux set-buffer \"$(wl-paste)\"; tmux paste-buffer"
bind-key -T copy-mode-vi 'v' send -X begin-selection
bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "${copyCommand}" # 'xclip -sel clip -i'
bind-key -T copy-mode-vi Y send-keys -X copy-end-of-line "${copyCommand}" #"xclip -selection c"
set -g set-clipboard on

# Vimish nav
unbind-key h
bind-key h select-pane -L
unbind-key j
bind-key j select-pane -D # Similar to 'C-w j' to navigate windows in Vim
unbind-key k
bind-key k select-pane -U
unbind-key l
bind-key l select-pane -R

set -g default-command zsh

# start with window 1 (instead of 0)
set -g base-index 1

# start with pane 1
set -g pane-base-index 1

# automatically number windows to stay near each other
set -g renumber-windows on

# Try tmux-256color first, else screen-256color
if-shell "infocmp tmux-256color >/dev/null 2>&1" \
  "set -g default-terminal tmux-256color"  \
  "set -g default-terminal screen-256color"

set-option -ga terminal-overrides ",xterm-256color:Tc"

# source config file
bind r source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"

# history
set -g history-limit 9999

# allow terminal scrolling
set-option -g terminal-overrides 'xterm*:smcup@:rmcup@'

# C-a C-a for last active window
bind-key C-a last-window

# set escape delay time to zero
set -s escape-time 0

# allows windows not constrained to the size of the smallest window
setw -g aggressive-resize on

# window title string (uses statusbar variables)
set -g set-titles on
set -g set-titles-string '#T'

# status bar with load and time
set -g status-left-length 30
set -g status-right-length 60
set -g status-left '#[bg=#BFEBBF]#[fg=#3F3F3F]#{session_name}#[fg=#DCDCCC]#[bg=3F3F3F]'
set -g status-right '#[bg=#3F3F3F]#[fg=#DFAF8F]#{pane_current_command} @ #{pane_pid}   #(date +" %H:%M ") [#h###S:#I:#P]'
set -g window-status-format '#[fg=#707880]#I#F #[fg=#C5C8C6]#W '
set -g window-status-format '#[fg=#707880]#I#F #[fg=#C5C8C6]#W '
set -g window-status-current-format '#[bg=#8CD0D3]#[fg=#3F3F3F]#I#F #W#[fg=#8CD0D3] '

# Activity monitoring
setw -g monitor-activity on
set -g visual-activity on

# General status stuff
set -g status-position bottom

set -g status on

# Don't renumber windows when I close them!
set -g renumber-windows off

# how often to update the status bar
set -g status-interval 2

# default statusbar colors
set -g status-fg white
set -g status-bg black

# pane-swapping shortcuts
# pane movement
bind-key J command-prompt -p "join pane from:"  "join-pane -s '%%'"
bind-key S command-prompt -p "send pane to:"  "join-pane -t '%%'"

# New window in the current directory
bind-key c neww -c "#{pane_current_path}"
# New window in the home directory
bind-key C neww
# Show a pretty selector between tmux panes
bind-key S choose-session
bind-key L next-layout
bind-key H previous-layout

# Automatically rename windows
set-window-option -g automatic-rename on

# set the update change interval
set-window-option -g xterm-keys off
bind-key m command-prompt -p "man >" "new-window 'exec man %%'"
bind-key b command-prompt -p "ranger >" "new-window 'exec ranger %%'"

bind-key C-s \
  if-shell "(($(tmux display -p '#{session_many_attached}') > 0))" \
    choose-session \
    "run-shell \"tmux choose-session \\\"switch-client -t '%%'; kill-session -t '$(tmux display -p '#S')'\\\"\""

set -g cursor-style bar            # block | bar | underline
    '';
  };
}
