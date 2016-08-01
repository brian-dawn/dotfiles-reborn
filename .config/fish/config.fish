# Path to Oh My Fish install.
set -gx OMF_PATH $HOME/.local/share/omf

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

####################
# Custom Functions #
####################

function add_to_path --description 'Persistently prepends paths to your PATH'
  set --universal fish_user_paths $fish_user_paths $argv
end

#########################
# Environment Variables #
#########################

set -x TERM xterm-256color


# Golang
set -x GOPATH $HOME/.go

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
