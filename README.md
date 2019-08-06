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
yay -S postgresql pgcli pgadmin4 pgmodeler
pip install --user pgcli [--upgrade]
yay -S shellcheck
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

```bash
# Activete/deactivate Python virtual environment
python -m venv <env>
source <env>/bin/activate
deactivate
# ./requirements.txt
awscli==1.16.209
cfn-lint==0.22.4
black==19.3b0
flake8==3.7.7
pylint==2.3.1
pytest==5.0.1
pytest-cov==2.7.1
# Install dependencies
pip install -r requireemnts.txt
# ./bin/run.sh
#!/usr/bin/env bash

set -eux

readonly LINE_LENGTH=88
readonly TEMPLATE=stack.yaml
readonly TEST=unit_test.py
readonly TARGET=main.py

# Validate Bash scripts
shellcheck -e <error_code> bin/*.sh
# Validate CloudFormation templates
aws cloudformation validate-template --template-body file://$TEMPLATE
cfn-lint $TEMPLATE
# Validate Python code
black --line-length $LINE_LENGTH $TARGET
flake8 --max-line-length=$LINE_LENGTH $TARGET
pylint --max-line-length=$LINE_LENGTH --disable=<error_code> $TARGET
pytest -x -v -s --disable-pytest-warnings \
    --cov $TARGET --cov-report term --cov-report html $TEST
python $TARGET
```

# Node environment

```bash
# Install NVM
yay -S nvm
# Install Node.js
nvm ls-remote
nvm ls
nvm install <version>
nvm alias default <version>
# Install Yarn
npm install yarn -g
# ./package.json
{
    "name": "typescript-basics",
    "version": "0.1.0",
    "description": "TypesScript basics",
    "main": "main.js",
    "author": "Volodymyr Prokopyuk",
    "license": "MIT",
    "private": true,
    "devDependencies": {
        "typescript": "3.5.3",
        "prettier": "1.18.2",
        "tslint": "5.18.0"
    }
}
# Install dependencies
yarn install
# Configure TSLint
./node_modules/.bin/tslint --init
# ./bin/run.sh
#!/usr/bin/env bash

set -eux

export PATH=./node_modules/.bin:$PATH
readonly LINE_LENGTH=88
readonly TAB_WIDTH=4
readonly TARGET=main

prettier --print-width $LINE_LENGTH --tab-width $TAB_WIDTH --no-bracket-spacing \
    --arrow-parens always --trailing-comma es5 --write $TARGET.ts
tslint --format verbose $TARGET.ts
tsc --target es6 $TARGET.ts
node $TARGET.js
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
ssh-add -l
```
