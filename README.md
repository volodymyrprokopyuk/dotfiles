# .dotfiles installation

```zsh
# Install yay with pacman from AUR
pacman -S yay
# Enable color in pacman/yay output
sudo sed -i -e 's/^#Color$/Color/' /etc/pacman.conf
# Install software with yay from core, extra, community and AUR repositories
sudo pacman-mirrors --fasttrack 5
sudo pacman -Syyu
# Update installed software and clean up unused packages
yay -Syu && yay -Sc
# Install tools
yay -S base-devel
yay -S adobe-source-code-pro-fonts ttf-jetbrains-mono ttf-fira-code
yay -S tmux zsh emacs xsel
yay -S the_silver_searcher fzf exa bat fd git-delta mlocate lscolors-git
# Clone the dotfiles repository into ~
git clone git@github.com:volodymyrprokopyuk/dotfiles.git ~/.dotfiles
# Configure zsh (log out, then log in)
chsh -s $(which zsh)
# Install applications
yay -S inkscape plantuml
yay -S dropbox
```

# pacman/yay usage

```zsh
# Update the repository database
yay -Sy
# Upgrade the system
yay -Syu
# Show packages to upgrade
yay -Qu
# Search the remote repository database for a package
yay -Ss[i] <package>
# Query the local repository database for the installed package
yay -Qs[i] <package>
# Install a package
yay -S <package>
# Remove a package, its configuration and dependencies
yay -Rsn <package>
# Install specific package version (downgrade)
yay -U https://archive.archlinux.org/packages/f/firefox/firefox-68.0.2-1-x86_64.pkg.tar.xz
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

# R environment

```zsh
# Install R
yay -S gcc-fortran openblas littler r
# Install package
install.packages("ggplot2")
# Update packages
update.packages()
# Load package
library(ggplot2)
# Remove package
remove.packages("ggplot2")
```

# Scheme environment

```zsh
yay -S gauche
```

# PostgreSQL environment

```zsh
# Install PostgreSQL server, client and tools
yay -S postgresql
# Create PostgreSQL data directory
sudo mkdir /var/lib/postgres/data
sudo chown -R postgres:postgres /var/lib/postgres/data
sudo chmod 755 -R /var/lib/postgres/*
# Initialize PostgreSQL database cluster
sudo su postgres
initdb --locale en_US.UTF-8 --encoding UTF-8 -D /var/lib/postgres/data
exit
# Enable/disable/start/stop/restart/status PostgreSQL database service
sudo systemctl enable|disable|start|stop|restart|status postgresql.service
# Show PostgreSQL log
journalctl -u postgresql.service
# Create database user
psql postgres postgres
CREATE USER vlad WITH PASSWORD 'vlad' SUPERUSER;
exit
# Create database
psql postgres vlad
CREATE DATABASE playground WITH OWNER vlad;
exit
# Connect to database
psql playground vlad
# Dump database schema
pg_dump -U vlad --schema-only playground > playground_dump_schema.sql
# Restore database schema
psql -h localhost -p 5432 -f playground_dump_schema.sql \
    -v ON_ERROR_STOP=1 -v ECHO=queries playground vlad
```

## psql commands

- General `\c`, `\conninfo`, `\du`, `\l`, `\dn`, `\g`, `\r`, `\q`
- Relations `\d`, `\dtvmi`
- Functions `\df`, `\dfnptaw`, `\sf`
- Editing `\e`, `\ef`


# Docker environment

```zsh
# Install and configure docker
yay -S docker
sudo groupadd docker
sudo usermod -G docker -a $USER
newgrp docker
sudo systemctl enable docker.service
sudo systemctl start docker.service
```

# Node environment

```zsh
# Install NVM
yay -S nvm
# Install Node.js
nvm ls-remote
nvm ls
nvm install <version>
nvm alias default <version>
# Install Yarn
npm install yarn -g
# Define dependencies
yarn init
# ./package.json
{
    "name": "typescript-ads",
    "version": "0.1.0",
    "description": "Algorithms and data structures in TypeScript",
    "main": "index.js",
    "repository": "https://github.com/volodymyrprokopyuk/typescript-ads",
    "author": "Volodymyr Prokopyuk <volodymyrprokopyuk@gmail.com>",
    "license": "MIT",
    "private": true,
    "devDependencies": {
        "prettier": "1.18.2",
        "eslint": "6.8.0",
        "typescript": "3.5.3",
        "jest": "24.9.0",
        "@types/jest": "24.0.18"
    }
}
# Install dependencies
yarn install
# Configure TSLint
./node_modules/.bin/eslint --init
```

# Kafka environment

```zsh
# Install Kafka broker, Zookeeper server and Kafka client
yay -S kafka kafkacat
# Enable/disable/start/stop/restart/status Zookeeper server
sudo systemctl enable|disable|start|stop|restart|status zookeeper.service
# Enable/disable/start/stop/restart/status Kafka broker
sudo systemctl enable|disable|start|stop|restart|status kafka.service
# Show Kafka broker metadata
kat -L
```

# BaseX environment

```zsh
# Install BaseX server and client
yay -S basex
# Create BaseX user and group
sudo useradd -M -U -c 'BaseX XML database server' basex
# Create BaseX data and log directories and configuration file
sudo mkdir -p /var/lib/basex/data /var/lib/basex/log
sudo cp .basex /var/lib/basex
sudo chown -R basex:basex /var/lib/basex
sudo chmod 755 -R /var/lib/basex
# Install systemd unit file
sudo cp basex.service /etc/systemd/system
# Enable/disable/start/stop/restart/status BaseX database service
sudo systemctl enable|disable|start|stop|restart|status basex.service
# Show BaseX log
journalctl -u basex.service
# Create database user
basexclient -U admin -P admin -c "CREATE USER vlad (vlad)" \
    -c "GRANT ADMIN ON * TO vlad"
# Create database
basexclient -U vlad -P vlad -c "CREATE DATABASE bookstore" \
    -c "ADD TO bookstore bookstore.xml"
# Connect to database
basexclient -U vlad -P vlad -c "OPEN bookstore" "/*"
```

# System environment

## Users and groups management

```zsh
# Show users
cat /etc/passwd
# Show groups
cat /etc/group
# Show groups that user belongs to
groups $USER
```

## systemd management

```zsh
# Show services
systemctl [list-units] --type=service --state=active
# Manage service
sudo systemctl enable|start|status|stop|restart|disable $SERVICE.service
```

## SSH configuration

Install and use locally generated SSH key on a remote server
```zsh
# Generate SSH key pair locally
# Provide SSH key file location ($HOME/.ssh/id_rsa_<provider>) and passphrase
# File parmissions
chmod 700 ~/.ssh && chmod 600 ~/.ssh/id_*
ssh-keygen -t rsa -b 4096 -C "volodymyrprokopyuk@gmail.com"
ssy-keygen -t ed25519 -C "volodymyrprokopyuk@gmail.com"
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
# Test SSH connection using SSH key
ssh -T <alias>
```

Add private SSH key identities to the SSH authentication agent
```zsh
# Start SSH authentication agent
eval $(ssh-agent)
# Add private SSH key identities to the SSH authentication agent. Provide passphrase
ssh-add ~/.ssh/id_rsa*~*.pub
# Show added to the SSH authentication agent private SSH key identities
ssh-add -l
```

## Format USB drive

```zsh
df -h
sudo umount /dev/sdc1
sudo mkfs.ext4 /dev/sdc1
```

## VirtualBox Manjaro configuration

```zsh
# Video configuration: 128MB, VBoxSVGA, 3D acceleration
yay -S linux419-virtualbox-guest-modules virtualbox-guest-utils
sudo usermod -G vboxsf -a $USER
newgrp vboxsf
sudo systemctl enable vboxservice.service
# Shared folder configuration: automount, make permanent
sudo VBoxControl sharedfolder list # Manjaro device
sudo mount -t vboxsf Manjaro /home/vlad/Manjaro -o uid=$(id -u),gid=$(id -g)
# Reboot
```
