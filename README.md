# Managing the configs

To initialize I ran:
```
git init --bare $HOME/.dotfiles-repo
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'
config config status.showUntrackedFiles no
```

Now I can do things like....

```
config status
config add .vimrc
config commit -m "Add vimrc"
config add .config/redshift.conf
config commit -m "Add redshift config"
config push
```

Special thanks to StreakyCobra on `https://news.ycombinator.com/item?id=11070797`



Now on new machines we can clone with:

```
git clone --bare [your-dotfiles-repo] $HOME/.dotfiles-repo
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=$HOME'
config checkout -f
config config status.showUntrackedFiles no
```

# Usability

## tmux

## kwm

