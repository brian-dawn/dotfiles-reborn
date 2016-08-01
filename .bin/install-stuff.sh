#!/bin/bash
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'

curl -L https://github.com/oh-my-fish/oh-my-fish/raw/master/bin/install | fish

# Reset the config directory because oh my fish is evil.
config checkout .config

# Tmux pkg manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Install emacs pkg management
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

# Install vim pkg management
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install base16 themes for fish
git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell

# Install fisher - yet another fish package manager...
curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher

# Install fzf - with fish integration...
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
$HOME/.fzf/install

# Install fzf fish plugin.
fish -c fisher fzf


