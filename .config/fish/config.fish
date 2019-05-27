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

# Haskell
alias ghc='stack exec -- ghc'
alias ghci='stack exec -- ghci'

# Clojure stuff
alias cljfmt='git diff --name-only | xargs lein cljfmt fix'

######################
# Path Modifications #
######################

add_to_path $HOME/.bin
add_to_path /usr/local/bin
add_to_path $HOME/kidblog/kb-api-server/scripts/bin

# Emacs
add_to_path $HOME/.cask/bin

# Golang
add_to_path $GOPATH/bin

# Rust
add_to_path $HOME/.cargo/bin

# Haskell/stack
add_to_path $HOME/.cabal/bin
add_to_path $HOME/.local/bin # prefer stack

################
# Custom Theme #
################

# Base16 Shell
if status --is-interactive
  eval sh $HOME/.config/base16-shell/scripts/base16-eighties.sh
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


# set -l base03  "--bold black"
# set -l base02  "black"
# set -l base01  "--bold green"
# set -l base00  "--bold yellow"
# set -l base0   "--bold blue"
# set -l base1   "--bold cyan"
# set -l base2   "white"
# set -l base3   "--bold white"
# set -l yellow  "yellow"
# set -l orange  "--bold red"
# set -l red     "red"
# set -l magenta "magenta"
# set -l violet  "--bold magenta"
# set -l blue    "blue"
# set -l cyan    "cyan"
# set -l green   "green"


# set fish_color_autosuggestion 505050 fda331
# set fish_color_command 6fb3d2 d381c3
# set fish_color_comment fb0120
# set fish_color_cwd a1c659
# set fish_color_cwd_root fb0120
# set fish_color_error fb0120 --bold
# set fish_color_escape 76c7b7
# set fish_color_history_current 76c7b7
# set fish_color_match 76c7b7
# set fish_color_normal e0e0e0
# set fish_color_operator 76c7b7
# set fish_color_param 6fb3d2 76c7b7
# set fish_color_quote b3643c
# set fish_color_redirection e0e0e0
# set fish_color_search_match --background=303030
# set fish_color_selection --background=303030
# set fish_pager_color_completion e0e0e0
# set fish_pager_color_description 505050 fda331
# set fish_pager_color_prefix 76c7b7
# set fish_pager_color_progress 76c7b7



set -g fish_color_normal      $base0
set -g fish_color_command     $base0
set -g fish_color_quote       $cyan
set -g fish_color_redirection $base0
set -g fish_color_end         $base0
set -g fish_color_error       $red
set -g fish_color_param       $blue
set -g fish_color_comment     $base01
set -g fish_color_match       $cyan
set -g fish_color_search_match "--background=$base02"
set -g fish_color_operator    $orange
set -g fish_color_escape      $cyan

# Used by fish_prompt

set -g fish_color_hostname    $cyan
set -g fish_color_cwd         $yellow
set -g fish_color_git         $green
