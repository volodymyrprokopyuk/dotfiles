# Configuration for

1. [zsh](http://www.zsh.org/)
1. [tmux](http://tmux.github.io/)
1. [emacs](http://www.gnu.org/software/emacs/)
1. [vscode](https://code.visualstudio.com/)
1. [vifm](http://vifm.info/)
1. [git](https://git-scm.com/)
1. [tig](https://github.com/jonas/tig)
1. [psql](http://www.postgresql.org/docs/9.4/static/app-psql.html)
1. [eslint](http://eslint.org/)

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
- [ninja](https://ninja-build.org/)
- [tmux](https://tmux.github.io/)
- [emacs](https://www.gnu.org/software/emacs/)
- [vscode](https://code.visualstudio.com/)
- [vifm](http://vifm.info/)
- [git](https://git-scm.com/)
- [tig](https://github.com/jonas/tig)
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
