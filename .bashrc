#alias config='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'
#config config status.showUntrackedFiles no

#[ -f ~/.fzf.bash ] && source ~/.fzf.bash

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi
