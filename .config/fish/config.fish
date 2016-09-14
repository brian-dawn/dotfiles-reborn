####################
# Custom Functions #
####################

function add_to_path --description 'Persistently prepends paths to your PATH'
  set -x PATH $argv $PATH
end

#########################
# Environment Variables #
#########################

set -x TERM xterm-256color


# Golang
set -x GOPATH $HOME/.go

# Rust
set -x CARGO_HOME $HOME/.cargo

###########
# Aliases #
###########

# Random
alias ll 'ls -l'

# Git aliases
alias gg  'git log --oneline --graph'
alias gs  'git status'
alias gco 'git checkout'
alias gh  'git rev-parse HEAD'

# Emacs
alias emacs 'emacs -nw'

alias vim 'nvim'

######################
# Path Modifications #
######################

add_to_path $HOME/.local/bin
add_to_path $HOME/.bin
add_to_path /usr/local/bin

# Emacs
add_to_path $HOME/.cask/bin

# Golang
add_to_path $GOPATH/bin

# Rust
add_to_path $HOME/.cargo/bin

################
# Custom Theme #
################

# Base16 Shell
if status --is-interactive
  eval sh $HOME/.config/base16-shell/scripts/base16-tomorrow-night.sh
end

# Disable greeting.
set fish_greeting ""

switch (uname)
    case Darwin
            # Do nothing..
    case Linux
            # Loonux fixes
            setxkbmap -option 'caps:ctrl_modifier'
            xcape -e 'Caps_Lock=Escape'
end


#######################
# Dotfiles Management #
#######################
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'
config config status.showUntrackedFiles no
set fish_color_autosuggestion 505050 fda331
set fish_color_command 6fb3d2 d381c3
set fish_color_comment fb0120
set fish_color_cwd a1c659
set fish_color_cwd_root fb0120
set fish_color_error fb0120 --bold
set fish_color_escape 76c7b7
set fish_color_history_current 76c7b7
set fish_color_match 76c7b7
set fish_color_normal e0e0e0
set fish_color_operator 76c7b7
set fish_color_param 6fb3d2 76c7b7
set fish_color_quote b3643c
set fish_color_redirection e0e0e0
set fish_color_search_match --background=303030
set fish_color_selection --background=303030
set fish_pager_color_completion e0e0e0
set fish_pager_color_description 505050 fda331
set fish_pager_color_prefix 76c7b7
set fish_pager_color_progress 76c7b7
