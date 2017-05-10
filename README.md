# Installation

Install utilities:

```bash
sudo apt-get install xsel libxml2-utils
```

Install tools:

```bash
sudo apt-get install git silversearcher-ag htop unrar
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

Install Emacs tools:

```elisp
M-x package-install RET flycheck
```

Install Java

```bash
sudo apt-get install openjdk-8-jre openjdk-8-jdk
```

Install Node

```bash
nvm ls-remote
nvm ls
nvm install <version>
nvm alias default <version>
```

Install Node tools

```bash
npm install -g jsonlint js-yaml
```

# Update & Configuration

```bash
cd ~/dotfiles
./play
```
