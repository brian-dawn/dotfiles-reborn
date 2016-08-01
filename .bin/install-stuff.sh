#!/bin/bash


# Tmux pkg manager
rm -rf ~/.tmux/plugins/tpm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Install emacs pkg management
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
cd ~/.emacs.d
cask
cd -

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
rm -rf ~/.config/fisherman
curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher

# Install fzf - with fish integration...
rm -rf ~/.fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
yes | ~/.fzf/install

# Clear fisher cache
rm -rf ~/.cache/fisherman
# Install fzf fish plugin.
fish -c fisher fzf
fish -c fisher oh-my-fish/chain

###################
#     Cleanup     #
###################
echo 'Cleanup!'

# Cleanup modifications made by the various installs.
/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME checkout ~/.bashrc

