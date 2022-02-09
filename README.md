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
yay --noconfirm -Syu && yay --noconfirm -Sc && yay --noconfirm -Yc
# Install tools
yay -S base-devel
yay -S adobe-source-code-pro-fonts ttf-jetbrains-mono ttf-fira-code
yay -S tmux zsh emacs xsel
yay -S fd ripgrep fzf exa bat git-delta lscolors-git mlocate
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
# Clean unneeded dependencies
yay -Yc
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

# Zinit

```zsh
mkdir -p ~/.local/share/zinit
git clone https://github.com/zdharma-continuum/zinit.git ~/.local/share/zinit/zinit.git
zinit self-update # zinit
zinit update # packages
```

```
Command line editing
Ctrl-a/Ctrl-e beginning/end of line
Alt-f/Alt-b forward/backward word
Ctrl-w delete word backword
Ctrl-x, e edit command line
Ctrl-l clear screen
```

# Doom Emacs

```zsh
git clone https://github.com/hlissner/doom-emacs.git ~/.emacs.d
doom install # packages
doom sync # config
doom upgrade # doom and packages
```

## Motions (content)

- `^|$` beginning / end of the current line content
- `0` 0 position of the current line

## Navigation (sniping)

- `f|F` current line 1-char snipe forward / backwards
- `t|T` current line 1-char exclusive snipe forward / backwards
- `s|S` current line 2-char snipe forward / backwards
- `;|,` repeat last snipe forward / backwards
- `g s s` visible screen 2-char snipe
- `g s /` visible screen timer snipe

## Search

- `*|#` search word under cursor forward / backwards
- `/|?` incremental search forward / backwards
- `n|N` repeat last search forward / backwards
- `:s/pattern/replace/gc` substitute pattern with replace (global, confirm)
- `&` repeat last substitution
- `SPC s s` search buffer
- `SPC s S` search buffer (word under cursor)
- `SPC /` search project
- `SPC SPC` find file in project
- `SPC s f` locate file in system

## Editing

Repeat, undo / redo

- `.` repeat the last change
- `u` undo the last change
- `C-r` redo the last undone change

Insertion

- `i|I` insert at cursor / at the beginning of current line
- `a|A` append at cursor / at the end of current line
- `o|O` open new line below cursor / above cursor

Change

- `c<move>|C` change till move / till the end of current line

Deletion

- `x` delete character under cursor
- `d<move>|D` delete till move / till the end of current line
- `dd` delete current line

Indentation

- `>|< <move>` indent / dedent vertical move
- `>>|<<` indent / dedent current line

## Buffers

- `SPC ,` switch buffer
- `SPC <backtick>` switch to last buffer


```
Evil mode
Tab complete the common part of suggestion list
Enter complete with the selected suggestion
C-SPC trigger completion window
M-q format width of the selected text
Ctrl-y paste into the mini buffer
Motions within a buffer
  Word/backwards/end: w/W, b/B, e/E, ge/gE
  Beginning/end of line: 0, ^, $,
  Search single char in current line: f/F, t/T -> ;/,
  Search regexp in a buffer: /?, */# -> n/N
Text objects: i/a w/s/p/t '/"/` )/}/]/>
  Change inside: ci_
  Delete around: da_
Operator + motion/text object: y, c, d, >, <, =, gc + w, it
Operator + operator: acts on the current line: cc, dd, >>, <<, ==, gcc, guu, gUU, g~~
Marks: m[mM]
  m mark in a buffer
  M global mark
  `m (go to marked position)
Change list in a buffer: g;/g,
Jump list between buffers in a window: Ctrl-o/Ctrl-i
Registers: "ryy, "rp
  r overwrite
  R append
Shift-r (replace mode: . A Shift-r , a)
q: (query and edit ex command history)
q/ (query and edit search history)
Insert mode/command line mode
  Alt-b, Alt-f (backwards, forward word)
  Ctrl-w (delete word backwards)
  Ctrl-r "/0 (paste unnamed/yank register)
Visual mode (should only be used when normal mode standard motions are not enough)
  o (other end of selection)
  Ctrl-v $ (ragged selection)
F1 k <key> show key binding
Evil surround
Normal mode: ys, cs, ds
Visual mode: S
Evil commenter
gcc comment a line
gc<movement> comment a movement
gc<a/i>(/[/{ comment outside/inside parentheses/brackets/braces
```

# Nim environment

```zsh
yay -S choosenim
choosenim versions
choosenim [update] stable | <version>
choosenim show
choosenim remove <version>
nimble refresh # package list
nimble install <package>[@<version>]
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
# Install and configure Docker
yay -S docker
sudo groupadd docker
sudo usermod -G docker -a $USER
newgrp docker
sudo systemctl enable docker.service
sudo systemctl start docker.service
# Image management
docker build <user>/<image>:<version> .
docker images -a
docker rmi <image_id>
docker system prune [-a]
# Network management
docker network create <networ>
docker network ls <network>
docker network rm <network>
# Container management
docker run --rm -it [-d] --name <container> --network <network> \
  -e ENV=value \
  --mount type=bind,src=host,dst=container,[readonly] \
  -p host:container \
  <image> <command>
docker ps -a
docker ps -qf "name=^pattern$"
docker rm <container_id>
# Execute a command inside a container
docker exec -it [-u 0] <container_id> ls
docker exec -it <container_id> sh -c 'ls /*'
docker exec -i <container_id> sh -c 'cat container_file' < host_file
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
