#!/bin/bash


# Tmux pkg manager
rm -rf ~/.tmux/plugins/tpm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Install emacs pkg management
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

# Install vim pkg management
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

###################
# Fish Management #
###################

# Install base16 themes for fish
rm -rf ~/.config/base16-shell
git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell

# Install fisher - yet another fish package manager...
curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher

# Install fzf - with fish integration...
rm -rf ~/.fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
yes | ~/.fzf/install

# Install fzf fish plugin.
fish -c fisher fzf

###################
#     Cleanup     #
###################
echo 'Cleanup!'

# Cleanup modifications made by the various installs.
/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME checkout ~/.bashrc

rm -rf ~/.config/fish
/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME checkout ~/.config/fish
