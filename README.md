# Installation

Install build tools:

```bash
sudo apt-get install build-essential texinfo
```

Install utilities:

```bash
sudo apt-get install unrar xsel
```

Install tools:

```bash
sudo apt-get install git silversearcher-ag htop xmlstarlet jq
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

Install dotfiles:

```bash
git clone git@github.com:volodymyrprokopyuk/dotfiles.git ~/.dotfiles
```

Install Java:

```bash
sudo apt-get install openjdk-8-jre openjdk-8-jdk
```

Install Docker:
```bash
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu xenial stable"
sudo apt-get update
sudo apt-get install docker-ce
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker
```

Install Node:

```bash
nvm ls-remote
nvm ls
nvm install <version>
nvm alias default <version>
```

Install Node utilities:

```bash
npm install -g js-beautify eslint
```

# Update & Configuration

```bash
cd ~/.dotfiles
./play
```
