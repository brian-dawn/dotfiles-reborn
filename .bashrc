#!/usr/bin/env bash

# Fix the umask on WSL
if grep -q Microsoft /proc/version; then
    umask 002
fi

# Path to the bash it configuration
export BASH_IT="$HOME/.bash_it"

# Lock and Load a custom theme file.
# Leave empty to disable theming.
# location /.bash_it/themes/
export BASH_IT_THEME='sexy'

# (Advanced): Change this to the name of your remote repo if you
# cloned bash-it with a remote other than origin such as `bash-it`.
# export BASH_IT_REMOTE='bash-it'

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Set this to the command you use for todo.txt-cli
export TODO="t"

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=true



# Load Bash It
source "$BASH_IT"/bash_it.sh



# Config management
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'
/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME config status.showUNtrackedFiles no

export ANT_OPTS="-Xmx1024m -Xms512m"

export TERM=xterm-256color
export PKG_CONFIG_PATH="/usr/X11/lib/pkgconfig"
export SHELL=/bin/bash

export PATH=$HOME/.local/bin:$PATH
export PATH=$PATH:~/.bin
export PATH=/usr/local/bin:$PATH

# emoji fix
export LESS='--raw-control-chars'


# Emacs
export PATH=$PATH:$HOME/.cask/bin
alias emacs="emacs -nw"
alias nmacs="open -n /Applications/Emacs.app"

# Linux brew
export PATH=$HOME/.linuxbrew/bin:$PATH

# Golang
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

# Haskell
export PATH=$PATH:~/cabal/ghc-mod-sandbox/.cabal-sandbox/bin
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/.local/bin/:$PATH


# Ruby
export PATH="$PATH:$HOME/.rvm/bin"
export PATH=/usr/local/opt/ruby/bin:$PATH

# JS
# export PATH="$PATH:`yarn global bin`"

# RUST
source $HOME/.cargo/env
case `uname` in
  Darwin)
  export RUST_SRC_PATH=~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src
  ;;
  Linux)
  export RUST_SRC_PATH=$HOME/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
  ;;
esac


# Java/maven
export MAVEN_OPTS='-Xmx1024m -XX:MaxDirectMemorySize=1024m -XX:MaxPermSize=256m'

# Make xclip be pbcopy
alias xclip="xclip -selection c"

alias ll='ls -l'

# Git aliases
alias gg='git log --oneline --graph'
alias gs='git status'
alias gco='git checkout'
# Git current commit hash.
alias gh='git rev-parse HEAD'
alias git-fuck-it='git clean -d -X -f; git reset --hard'
alias git-prune-local='git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d'



export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
