# .dotfiles installation

```bash
# install yay with makepkg from git
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
# install software with yay from core, extra, community and AUR repositories
sudo pacman-mirrors --fasttrack 5 && sudo pacman -Syyu
yay -S adobe-source-code-pro-fonts tmux emacs the_silver_searcher fzf xsel diff-so-fancy mlocate
yay -S xmlstarlet jq pwgen apg openbsd-netcat
yay -S ansible docker
yay -S python-black flake8 python-pylint ptpython pgcli mycli
yay -S yed sdedit
yay -S intellij-idea-ce
yay -S dropbox google-chrome gnome-keyring skypeforlinux-preview-bin
# configure zsh
chsh -s $(which zsh)
# configure docker
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker
sudo systemctl start docker
# logout, login and then configure docker swarm
docker swarm init
```

# pacman/yay usage

```bash
# update the repository database
yay -Sy
# upgrade the system
yay -Syu
# search the remote repository database for a package
yay -Ss[i] <package>
# query the local repository database for the installed package
yay -Qs[i] <package>
# install a package
yay -S <package>
# remove a package, its configuration and dependencies
yay -Rsn <package>
# clean package cache
yay -Sc
# show explicitly installed packages
yay -Qe
# list all files owned by a package
yay -Ql <package>
# show which package the file belongs to
yay -Qo <file path>
# show system statistics
yay -Ps
# show dependency tree of a package
pactree <package>
```

# Python environment

Activete/deactivate python virtual environment
```bash
python -m venv <env>
source <env>/bin/activate
deactivate
```

# Node environment

Install Node
```bash
nvm ls-remote
nvm ls
nvm install <version>
nvm alias default <version>
```

Install Node utilities
```bash
npm install -g js-beautify eslint
```

# SSH configuration

Install and use locally generated SSH key on a remote server
```bash
# generate SSH key pair locally. Provide SSH key file location ($HOME/.ssh/id_rsa_<provider>) and passphrase
# file parmissions: ~/.ssh = 700, ~/.ssh/id_rsa* = 600
ssh-keygen -t rsa -b 4096 -C "volodymyrprokopyuk@gmail.com"
# copy SSH key to a remote host. Provide remote host username and password
ssh-copy-id -i ~/.ssh/id_rsa_<provider>.pub <username>@<host>
# connect to the remote host using SSH key but not password. Provide passphrase
ssh -i ~/.ssh/id_rsa_<provider> <username>@<host>
# configure SSH key alias in ~/.ssh/config
Host <alias>
    HostName <host>
    User <user>
    IdentityFile ~/.ssh/id_rsa<provider>
# connect to the remote host using SSH key alias. Provide passphrase
ssh <alias>
```

Add private SSH key identities to the SSH authentication agent
```bash
# start SSH authentication agent
eval $(ssh-agent)
# add private SSH key identities to the SSH authentication agent. Provide passphrase
ssh-add ~/.ssh/id_rsa*~*.pub
# show added to the SSH authentication agent private SSH key identities
ssh-add -L
```
