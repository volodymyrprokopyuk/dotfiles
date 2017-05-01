# Installation

Install utilities:

```bash
sudo apt-get install xsel
sudo apt-get install libxml2-utils
```

Install tools:

```bash
sudo apt-get install git
sudo apt-get install silversearcher-ag
sudo apt-get install unrar
```

Install Ansible:

```bash
sudo apt-get install openssh-server
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys
sudo apt-get install ansible
```

Install Emacs:

```bash
sudo add-apt-repository -y ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot
```

Install Zsh:

```bash
sudo apt-get install zsh
chsh -s $(which zsh)
```

Install Tmux:

```bash
sudo apt-get install tmux
```

Install dotfiles
```bash
cd ~
git clone git@github.com:volodymyrprokopyuk/dotfiles.git
```

# Update & Configuration

```bash
cd ~/dotfiles
./play
```
