# .dotfiles installation

```bash
# Install yay with makepkg from git
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
# Install software with yay from core, extra, community and AUR repositories
sudo pacman-mirrors --fasttrack 5 && sudo pacman -Syyu
yay -S adobe-source-code-pro-fonts tmux emacs the_silver_searcher fzf xsel diff-so-fancy mlocate
yay -S jq pwgen apg
yay -S python-black flake8 python-pylint ptpython cfn-lint
yay -S nvm
yay -S postgresql pgcli pgadmin4 pgmodeler
yay -S shellcheck
pip install --user pgcli [--upgrade]
yay -S idris swi-prolog
yay -S yed plantuml
yay -S intellij-idea-ce
yay -S dropbox google-chrome gnome-keyring skypeforlinux-preview-bin
yay -S youtube-dl
youtube-dl --extract-audio --audio-format mp3 --audio-quality 0 'URL'
# Update installed software and clean up unused packages
yay -Syu && yay -Sc
# Configure zsh
chsh -s $(which zsh)
# Install and configure docker
yay -S docker
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker
sudo systemctl start docker
# Logout, login and then configure docker swarm
docker swarm init
```

# pacman/yay usage

```bash
# Update the repository database
yay -Sy
# Upgrade the system
yay -Syu
# Search the remote repository database for a package
yay -Ss[i] <package>
# Query the local repository database for the installed package
yay -Qs[i] <package>
# Install a package
yay -S <package>
# Remove a package, its configuration and dependencies
yay -Rsn <package>
# Clean package cache
yay -Sc
# Show explicitly installed packages
yay -Qe
# List all files owned by a package
yay -Ql <package>
# Show which package the file belongs to
yay -Qo <file path>
# Show system statistics
yay -Ps
# Show dependency tree of a package
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

# PostgreSQL environment

```bash
# Initialize PostgreSQL database cluster
sudo su postgres
initdb --locale en_US.UTF-8 --encoding UTF-8 -D /var/lib/postgres/data
exit
# Start/enable PostgreSQL database service
sudo systemctl start|enable|status postgresql.service
# Create database user
sudo su postgres
psql
CREATE USER vld WITH PASSWORD 'vld' SUPERUSER;
exit
# Create database
psql postgres vld
CREATE DATABASE vlddb WITH OWNER vld;
exit
# Connect to database
pgcli [-h localhost] vlddb vld
```

# SSH configuration

Install and use locally generated SSH key on a remote server
```bash
# Generate SSH key pair locally
# Provide SSH key file location ($HOME/.ssh/id_rsa_<provider>) and passphrase
# File parmissions: ~/.ssh = 700, ~/.ssh/id_rsa* = 600
ssh-keygen -t rsa -b 4096 -C "volodymyrprokopyuk@gmail.com"
# Copy SSH key to a remote host. Provide remote host username and password
ssh-copy-id -i ~/.ssh/id_rsa_<provider>.pub <username>@<host>
# Connect to the remote host using SSH key but not password. Provide passphrase
ssh -i ~/.ssh/id_rsa_<provider> <username>@<host>
# Configure SSH key alias in ~/.ssh/config
Host <alias>
    HostName <host>
    User <user>
    IdentityFile ~/.ssh/id_rsa<provider>
# Connect to the remote host using SSH key alias. Provide passphrase
ssh <alias>
```

Add private SSH key identities to the SSH authentication agent
```bash
# Start SSH authentication agent
eval $(ssh-agent)
# Add private SSH key identities to the SSH authentication agent. Provide passphrase
ssh-add ~/.ssh/id_rsa*~*.pub
# Show added to the SSH authentication agent private SSH key identities
ssh-add -L
```
