#!/usr/bin/env bash


# Install fzf
# Say yes to everything, then C-T and C-R will use fzf.
# $ git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
# $ ~/.fzf/install

# install starship
# $ cargo install starship

# install emojis manjaro
# $ pacman -Sy noto-fonts-emoji

# Fix the umask on WSL
if grep -q Microsoft /proc/version; then
    umask 002
fi

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Config management
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'
/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME config status.showUNtrackedFiles no

export TERM=xterm-256color
export PKG_CONFIG_PATH="/usr/X11/lib/pkgconfig"
export SHELL=/bin/bash

export PATH=$HOME/.local/bin:$PATH
export PATH=$PATH:~/.bin
export PATH=/usr/local/bin:$PATH

# Python
export PATH="$HOME/.poetry/bin:$PATH"


# emoji fix
export LESS='--raw-control-chars'

# neovim config.
alias vim=nvim

# Use ripgrep for fzf so we respect .gitignore
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'

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

case `uname` in
  Darwin)

  alias ls='ls -G'
  alias ll='ls -lG'
  ;;
  Linux)

  alias ls='ls --color=auto'
  alias ll='ls --color=auto -l'
  ;;
esac


# Git aliases
alias gg='git log --oneline --graph'
alias gs='git status'
alias gco='git checkout'
# Git current commit hash.
alias gh='git rev-parse HEAD'
alias git-fuck-it='git clean -d -X -f; git reset --hard'
alias git-prune-local='git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d'

# Make it so we can save spaces in front of things to history.
export HISTCONTROL=erasedups


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

if [ -x "$(command -v rbenv)" ]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

eval "$(starship init bash)"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/brian/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/brian/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/brian/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/brian/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

