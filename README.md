# Configuration for

1. [zsh](http://www.zsh.org/)
1. [tmux](http://tmux.github.io/)
1. [emacs](http://www.gnu.org/software/emacs/)
1. [git](https://git-scm.com/)

# Installation

## Install from repository

```bash
sudo apt-get install openssh-server
sudo apt-get install xsel
sudo apt-get install libxml2-utils
sudo apt-get install unrar
sudo apt-get install jq
```

## Install Ansible

```bash
sudo add-apt-repository ppa:ansible/ansible
sudo apt-get update
sudo apt-get install ansible

cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

# /etc/ansible/hosts
[localhost]
localhost
```

## Install Emacs

```bash
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot
```

## Install from source

- [zsh](http://www.zsh.org/)
- [tmux](https://tmux.github.io/)
- [ag](https://github.com/ggreer/the_silver_searcher)

## Install zsh

```bash
which zsh | sudo tee -a /etc/shells
chsh -s $(which zsh)
```

## Install config files

```bash
git clone git@github.com:volodymyrprokopyuk/dotfiles.git
cd dotfiles
ninja
ninja update
```
