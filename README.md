# Initialization

```fish
# Disable login password
sudo groupadd -r autologin
sudo gpasswd -a $USER autologin
# /etc/lightdm/lightdm.conf
[Seat:*]
autologin-user=vlad

# Enable options in /etc/pacman.conf
Color CheckSpace VerbosePkgLists ParallelDownloads = 4
# Update package repositories
sudo pacman -Syyu
# Update installed packages and remove unused packages
yay --noconfirm -Syu && yay --noconfirm -Sc && yay --noconfirm -Yc

# Install packages
yay -S wezterm fish starship emacs-nativecomp
yay -S aspell aspell-en ttf-jetbrains-mono-nerd
yay -S fzf fd ripgrep eza bat git-delta vivid mlocate btop sd
yay -S lf zathura zathura-pdf-mupdf zathura-djvu pdfcpu feh mpv
yay -S nodejs pnpm bun-bin typescript typescript-language-server
yay -S go-ethereum solidity-bin
yay -S dropbox

# Install fish
chsh -s $(which fish)

# Install dotfiles
git clone git@github.com:volodymyrprokopyuk/dotfiles.git ~/.dotfiles
cd ~/.dotfiles && ./setup.fish --init all

# Install Doom Emacs
git clone https://github.com/hlissner/doom-emacs.git ~/.config/emacs
doom install # packages
doom sync # config
doom upgrade # Doom and packages
```

## i3wm

```zsh
# Set font scaling DPI (multiple of 96) in ~/.Xresources
Xcursor.theme: Qogir
Xcursor.size: 48
Xft.dpi: 192
# Install packages
yay -S xorg-xev i3status-rust i3lock-color maim
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

# Fish

Movements

- `C-f|b` forward/backward char
- `A-f|b` forward/backward word
- `C-a|e` beginning/end of line

Deletion

- `C-k|u` delete forward/backward line
- `C-w` delete backward word
- `C-z` undo the last edit

History

- `C-p|n` previous/next history command
- `C-r` search command history
- `C-g` cancel operation

Program

- `C-c` terminate program
- `C-d` end input stream
- `C-l` clear screen

Completion

- `Tab/S-Tab` forward/backward completion
- `M-m/,` forward/backward completion

# Doom Emacs

## Scrolling

- `C-e|y` forward/backward line
- `C-f|b` forward/backward full screen
- `C-d|u` forward/backward half screen

## Motions (content)

- `h|l` left / right character
- `[g]j|k` down / up [visual] line
- `w|b` beginning of the next / previous word + `W|B` on whitespace only
- `e|ge` end of the next / previous word + `E|gE` on whitespace only
- `^|$` beginning / end of the current line content
- `0` 0 position of the current line
- `%` jump between parentheses
- `zz` current line in the middle of the screen
- `H|M|L` move cursor to high / middle / low of the screen

Text objects

- `i|aw|s|p|t` inside / around word / sentence / paragraph / tag +
  `` ([{<"'``'">}]) ``

Surround

- `ys|S<obj>` add surrounding in normal / visual mode
- `c|ds<sur>` change / delete surrounding in normal mode

Visual mode

- `v|V|C-v` char / line / block visual selection
- `C-v $` ragged right visual selection
- `o` go to the other end of selection
- `gv` reselect the last visual selection

## Navigation (sniping)

- `f|F` current line 1-char snipe forward / backwrd
- `t|T` current line 1-char exclusive snipe forward / backwrd
- `s|S` current line 2-char snipe forward / backwrd
- `;|,` repeat last snipe forward / backwrd
- `g s s` visible screen 2-char snipe
- `g s /` visible screen timer snipe
- `g;|,` jump to last modification backwrd / forward (change list)
- `C-o|i` jump to last location  backwrd / forward (jump list)
- `` m|'|`{a-z} `` set mark / jump to mark line / column

## Search

- `*|#` search word under cursor forward / backwrd
- `/|?` incremental search forward / backwrd
- `n|N` repeat last search forward / backwrd
- `:s/pattern/replace/gc` substitute pattern with replace (global, confirm)
- `&` repeat last substitution on the current line
- `SPC s s` search buffer
- `SPC s S` search buffer (word under cursor)
- `SPC /` search project
- `SPC SPC` find file in project (git)
- `SPC .` find file in directory (no git)
- `SPC s f` locate file in system
- `q:|/|? j|k` query recent command line / forward / backwrd search

## Editing

- `:e` edit existing or new file
- `:w[a]` write file
- `C-x C-c` exit Doom Emacs

Repeat, undo / redo

- `.` repeat the last change
- `u` undo the last change
- `C-r` redo the last undone change

Yank / paste, registers

- `y<move>|Y` yank till move / till the end of current line
- `yy` yank current line
- `p|P` paste after / before cursor

Insertion

- `i|I` insert at cursor / at the beginning of current line
- `a|A` append at cursor / at the end of current line
- `o|O` open new line below cursor / above cursor

Change

- `c<move>|C` change till move / till the end of current line
- `cc` change current line
- `gu|U<move>` downcase / upcase till move
- `guu|UU` downcase / upcase current line
- `J` join lines

Deletion

- `x` delete character under cursor
- `d<move>|D` delete till move / till the end of current line
- `dd` delete current line

Indentation and formatting

- `>|<<move>` indent / dedent move
- `>>|<<` indent / dedent current line
- `=<move>` format move

Commenting

- `gc<move>` comment / uncomment move
- `gcc` comment / uncomment current line

## Buffers

- `SPC ,` switch buffer
- ``SPC ` `` switch to alternate buffer
- `C-x k` kill buffer

## Windows

- `C-w s|v` split window horizontal / vertical
- `C-w =` equalize windows
- `C-w w|h|j|k|l` cycle / move between windows
- `C-w c` close active window
- `C-w r` rotate windows
- `C-x 0|1` close other / active window

```
Evil mode
Tab complete the common part of suggestion list
Enter complete with the selected suggestion
C-SPC trigger completion window
M-q format width of the selected text
Ctrl-y paste into the mini buffer
Motions within a buffer
  Word/backwrd/end: w/W, b/B, e/E, ge/gE
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
  Alt-b, Alt-f (backwrd, forward word)
  Ctrl-w (delete word backwrd)
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

# Zathura

- `o|O` open / in current directory
- `R` reload
- `q` quit
- `C-g` cancel TODO
- `h|j|k|l` scroll left / down / up | right
- `J|K` next / previous page
- `C-f|b` forward / backwrd page
- `H|L` top / bottom of page
- `gg|G|<n>G` first / last / n page
- `a|s|d` fit page height / width / dual view
- `r` rotate
- `+|-|=` zoom in / out / original size
- `F11|F5` fullscreen / presentation mode
- `Tab|j+k|l+h` index view / down + up / expand + collapse entry
- `/|?|n+N` search forward / backwrd / next + previous match
- `m<a>|'<a>` mark page / go to marked page
- `convert in.jpg -rotate 90 -resize 1600 -crop 1450x2100+50+33 -threshold 60% -quality 70% cert.pdf`

# Mupdf

- `.|space ,|b` forward/backward page
- `4g` go to page 4
- `g|G` to to first/last page
- `[0-9]m|[0-9]t` mark page/top back to the marked page
- `+|-` zoom in/out
- `W|H` fit width/height
- `[|]` rotate page
- `f` toggle fullscreen
- `h|j|k|l` move left/down/up/right
- `r` reload file

# Network

``` zsh
lspci -v # network kernel module
sudo dmesg | grep <kernel module> # network interface
ip link show dev <network interface> # network interface status
ip link set <network interface> up | down # [dis]connect network interface
```

# Firefox

## Navigation

- `C-[|]` history backwrd / forward
- `C-R|C-F5` reload page / override cache

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

- General `\c`, `\conninfo`, `\du`, `\dx` `\l`, `\dn`, `\g`, `\r`, `\q`
- Relations `\d`, `\dtvmi`
- Functions `\df`, `\dfnptaw`, `\sf`
- Editing `\e`, `\ef`


# Docker environment

```zsh
# Install and configure Docker
yay -S docker docker-compose
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

# Docker compose
docker-compose -f <compose.yaml> up [-d]
docker-compose ps
docker-compose run <service> <command>
docker-compose stop
docker-compose down <container-name>
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
