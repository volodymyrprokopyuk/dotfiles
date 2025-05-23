# Initialization

# Commands

## Network

```fish
nmcli device # list devices => connections
nmcli connection # list connections => devices
nmcli connection up|down <connection> # adtivate/deactivate connection
nmcli radio wifi on|off # switch on/off WiFi
nmcli device wifi # list avaialble WiFi networks
nmcli --ask device wifi connect <network> # connect to network
```

## Mount

```fish
sudo fdisk -l # list devices
ll /dev/disk/by-uuid/* # list device UUIDs
sudo mkdir /run/media/HD1 # create a mount point
sudo mount -t ext4 /dev/sda3 /run/media/HD1 # mount a device
sudo mount -t vfat /dev/sdc1 /run/media/PD1
df -h # verify mount success; show storage space usage
sudo umount /run/media/HD1 # unmount a device
# /etc/fstab
UUID=aa295f1c-3f40-4a02-b91c-a57c657ec247 /run/media/HD1 ext4 defaults 0 0
UUID=cbb67744-1bb8-42b8-ba94-bb97071404b2 /run/media/SD1 ext4 defaults,nofail,x-systemd.device-timeout=1 0 0
sudo mount /run/media/HD1 # mount a device from fstab
sudo mount -a # mount all devices from fstab
du -hd 1 # show directory size to depth
```

## Format

```fish
df -h
sudo umount /dev/sdc1
sudo mkfs.ext4 /dev/sdc1
```

## Users and groups

```fish
# Show users
cat /etc/passwd
# Show groups
cat /etc/group
# Show groups that user belongs to
groups $USER
```

## systemd management

```fish
# Show services
systemctl [list-units] --type=service --state=active
# Manage service
sudo systemctl enable|start|status|stop|restart|disable $SERVICE.service
```

# Applications

## Fish

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

## Doom Emacs TODO

Scrolling

- `C-e|y` forward/backward line
- `C-f|b` forward/backward full screen
- `C-d|u` forward/backward half screen

Motions

/- `h|l` left / right character
/- `[g]j|k` down / up [visual] line
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
- `gv` re-select the last visual selection

Navigation

- `f|F` current line 1-char snipe forward / backwrd
- `t|T` current line 1-char exclusive snipe forward / backwrd
- `s|S` current line 2-char snipe forward / backwrd
- `;|,` repeat last snipe forward / backwrd
- `g s s` visible screen 2-char snipe
- `g s /` visible screen timer snipe
- `g;|,` jump to last modification backwrd / forward (change list)
- `C-o|i` jump to last location  backwrd / forward (jump list)
- `` m|'|`{a-z} `` set mark / jump to mark line / column

Search

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

Editing

- `:e` edit existing or new file
- `:w[a]` write file
- `C-x C-c` exit Doom Emacs

Repeat, undo/redo

- `.` repeat the last change
- `u` undo the last change
- `C-r` redo the last undone change

Yank/paste, registers

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

Buffers

- `SPC ,` switch buffer
- ``SPC ` `` switch to alternate buffer
- `C-x k` kill buffer

Windows

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

Operation

- `o|O` open recent files/in current directory
- `R` reload document
- `C-c|g` cancel operation
- `C-n` toggle status bar
- `F11|F5` toggle full screen/presentation mode
- `q` quit

Movements

- `h|j|k|l` scroll left/down/up/right
- `C-f|b`, `Space|S-Space` scroll forward/backward page
- `C-d/u` scroll forward/backward half page
- `J|K|H|L` scroll next/previous/top/bottom page

Navigation

- `gg|G|<n>G` go to first/last/n page
- `m<a>|'<a>` mark page/go to marked page
- `C-i|o` go to next/previous jump list item

Transformation

- `a|s|d` fit page height/width/dual view
- `+|-|=|<n>=` zoom in/out/original size/% size
- `r` rotate document

Search

- `/|?` search forward/backward
- `n|N` go to next/previous match

Index

- `Tab` toggle index view
- `j+k|l+h` move down/up, expand/collapse entry
- `L+H` expand/collapse all entries

- `convert in.jpg -rotate 90 -resize 1600 -crop 1450x2100+50+33 -threshold 60% -quality 70% cert.pdf`

# Environments

## PostgreSQL

```fish
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
