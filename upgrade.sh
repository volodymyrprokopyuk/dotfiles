#!/usr/bin/env bash

readonly DOTFILES_HOME=~/.dotfiles
readonly ZSH_HOME=~/.zsh
readonly EMACS_HOME=~/.emacs.d
readonly LOG=out.log

# Colors
readonly end="\e[0m"
# Normal color
readonly red="\e[31m"
readonly green="\e[32m"
readonly yellow="\e[33m"
readonly blue="\e[34m"
# Bold face
readonly redb="\e[1;31m"
readonly greenb="\e[1;32m"
readonly yellowb="\e[1;33m"
readonly blueb="\e[1;34m"
# Light color
readonly lred="\e[91m"
readonly lgreen="\e[92m"
readonly lyellow="\e[93m"
readonly lblue="\e[94m"
# Light color and bold face
readonly lredb="\e[1;91m"
readonly lgreenb="\e[1;92m"
readonly lyellowb="\e[1;93m"
readonly lblueb="\e[1;94m"

read -d "" USAGE <<EOF
Usage:
    ./upgrade.sh [-c] [-u] [targets]
Options:
    -h, --help
    -c, --config
    -u, --upgrade
Targets:
    common | git | tmux | zsh | emacs | aws
EOF

readonly MESSAGE="${lgreen}%s ${lblue}%s ${yellowb}%s${end}\n"
readonly FAILURE="${red}FAILURE${end}\n"

ALL_ACTION=true
ALL_TARGET=true

function read_options {
    while (( $# )); do
        case $1 in
            # Actions
            -h|--help)
                echo "$USAGE"
                shift
                exit
                ;;
            -c|--config)
                unset ALL_ACTION
                CONFIG_ACTION=true
                shift
                ;;
            -u|--upgrade)
                unset ALL_ACTION
                UPGRADE_ACTION=true
                shift
                ;;
            -*)
                echo "$USAGE"
                shift
                exit 1
                ;;
            # Targets
            common)
                unset ALL_TARGET
                COMMON_TARGET=true
                shift
                ;;
            git)
                unset ALL_TARGET
                GIT_TARGET=true
                shift
                ;;
            tmux)
                unset ALL_TARGET
                TMUX_TARGET=true
                shift
                ;;
            zsh)
                unset ALL_TARGET
                ZSH_TARGET=true
                shift
                ;;
            emacs)
                unset ALL_TARGET
                EMACS_TARGET=true
                shift
                ;;
            aws)
                unset ALL_TARGET
                AWS_TARGET=true
                shift
                ;;
            *)
                echo "$USAGE"
                shift
                exit 1
                ;;
        esac
    done
}

function common_config {
    local target=common
    local action=config

    printf "$MESSAGE" $target $action "Copying .editorconfig into ~"
    cp $DOTFILES_HOME/.editorconfig ~
    printf "$MESSAGE" $target $action "Copying config.py into ~/.ptpython"
    mkdir -p ~/.ptpython
    cp $DOTFILES_HOME/ptpython_config.py ~/.ptpython/config.py
    printf "$MESSAGE" $target $action "Copying .myclirc into ~"
    cp $DOTFILES_HOME/.myclirc ~
}

function git_config_common {
    local target=$1
    local action=$2

    printf "$MESSAGE" $target $action "Configuring Git options in ~/.gitconfig"
    git config --global user.name "Volodymyr Prokopyuk"
    git config --global user.email "volodymyrprokopyuk@gmail.com"
    git config --global core.excludesfile "~/.gitignore"
    git config --global push.default "simple"
    git config --global diff.algorithm "histogram"
    git config --global status.showUntrackedFiles "all"
    git config --global credential.helper "cache --timeout=86400"
    git config --global core.quotepath "false"
}

function git_config_aliases {
    local target=$1
    local action=$2

    printf "$MESSAGE" $target $action "Configuring Git aliases in ~/.gitconfig"
    git config --global alias.s "!git status -sb && git stash list"
    git config --global alias.d "diff"
    git config --global alias.ds "diff --stat"
    git config --global alias.dc "diff --cached"
    git config --global alias.dcs "diff --cached --stat"
    git config --global alias.dch "diff --check"
    git config --global alias.l 'log --all --graph --abbrev-commit --pretty=format:"%C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)"'
    git config --global alias.lf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\n"'
    git config --global alias.lsf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\n" -S'
    git config --global alias.lgf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\n" -G'
    git config --global alias.bm "branch --merged"
    git config --global alias.bnm "branch --no-merged"
    git config --global alias.bb "branch -a -vv"
    git config --global alias.a "!git add -A && git s"
    git config --global alias.cm "!git dch && git commit"
    git config --global alias.ch "checkout"
}

function git_config_diff {
    local target=$1
    local action=$2

    printf "$MESSAGE" $target $action "Configuring Git diff in ~/.gitconfig"
    git config --global core.pager "diff-so-fancy | less --tabs=4 -RFX"
    git config --global color.ui "true"
    git config --global color.diff-highlight.oldNormal "red bold"
    git config --global color.diff-highlight.oldHighlight "red bold 52"
    git config --global color.diff-highlight.newNormal "green bold"
    git config --global color.diff-highlight.newHighlight "green bold 22"
    git config --global color.diff.meta "yellow"
    git config --global color.diff.frag "magenta bold"
    git config --global color.diff.commit "yellow bold"
    git config --global color.diff.old "red bold"
    git config --global color.diff.new "green bold"
    git config --global color.diff.whitespace "red reverse"
}

function git_config {
    local target=git
    local action=config

    printf "$MESSAGE" $target $action "Copying .gitignore into ~"
    cp $DOTFILES_HOME/global_gitignore ~/.gitignore

    git_config_common $target $action
    git_config_aliases $target $action
    git_config_diff $target $action
}

function tmux_config {
    local target=tmux
    local action=config

    printf "$MESSAGE" $target $action "Copying .tmux.conf into ~"
    cp $DOTFILES_HOME/.tmux.conf ~
}

function zsh_config {
    local target=zsh
    local action=config

    printf "$MESSAGE" $target $action "Copying .zshrc into ~"
    cp $DOTFILES_HOME/.zshrc ~
}

function zsh_upgrade {
    local target=zsh
    local action=upgrade

    read -d "" zsh_extensions <<EOF
https://github.com/denysdovhan/spaceship-prompt.git
https://github.com/zsh-users/zsh-completions.git
https://github.com/zsh-users/zsh-autosuggestions.git
https://github.com/zsh-users/zsh-syntax-highlighting.git
EOF

    mkdir -p $ZSH_HOME

    while read zsh_extension; do
        printf "$MESSAGE" $target $action "Installing $zsh_extension into ~/.zsh"
        local zsh_extension_dir=${zsh_extension##*/}
        zsh_extension_dir=${zsh_extension_dir%.git}
        local zsh_extension_path=$ZSH_HOME/$zsh_extension_dir
        cd $ZSH_HOME
        cd $zsh_extension_path && git pull >>$LOG || git clone $zsh_extension \
            || printf $FAILURE
    done <<<$zsh_extensions
}

function emacs_config {
    local target=emacs
    local action=config

    printf "$MESSAGE" $target $action "Copying .emacs into ~"
    cp $DOTFILES_HOME/.emacs ~
    printf "$MESSAGE" $target $action "Copying emacs.el into ~/.emacs.d"
    mkdir -p $EMACS_HOME
    cp $DOTFILES_HOME/emacs.el $EMACS_HOME
}

function emacs_upgrade_web {
    local target=$1
    local action=$2

    read -d "" emacs_packages <<EOF
https://www.emacswiki.org/emacs/download/goto-chg.el
https://www.emacswiki.org/emacs/download/key-chord.el
EOF

    while read emacs_package; do
        printf "$MESSAGE" $target $action "Installing $emacs_package into ~/.emacs.d"
        local emacs_package_dir=${emacs_package##*/}
        local emacs_package_path=$EMACS_HOME/$emacs_package_dir
        mkdir -p $emacs_package_path
        cd $emacs_package_path
        curl -s -O $emacs_package || printf $FAILURE
    done <<<$emacs_packages
}

function emacs_upgrade_git {
    local target=$1
    local action=$2

    read -d "" emacs_packages <<EOF
https://github.com/cask/epl.git
https://github.com/lunaryorn/pkg-info.el.git
https://github.com/magnars/s.el.git
https://github.com/magnars/dash.el.git
https://github.com/jwiegley/emacs-async.git
https://github.com/purcell/exec-path-from-shell.git
https://github.com/bbatsov/zenburn-emacs.git
https://github.com/Fanael/rainbow-delimiters.git
https://github.com/milkypostman/powerline.git
https://github.com/TheBB/spaceline.git
https://github.com/Fuco1/smartparens.git
https://github.com/emacs-helm/helm.git
https://github.com/syohex/emacs-helm-ag.git
https://github.com/company-mode/company-mode.git
https://github.com/abo-abo/avy.git
https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode.git
https://github.com/mooz/js2-mode.git
https://github.com/fxbois/web-mode.git
https://github.com/smihica/emmet-mode.git
https://github.com/idris-hackers/idris-mode.git
https://github.com/spotify/dockerfile-mode.git
https://github.com/skuro/plantuml-mode.git
https://github.com/jrblevin/markdown-mode.git
https://github.com/yoshiki/yaml-mode.git
https://github.com/emacs-evil/evil.git
https://github.com/emacs-evil/evil-surround.git
https://github.com/redguardtoo/evil-nerd-commenter.git
https://github.com/edkolev/evil-goggles.git
EOF

    while read emacs_package; do
        printf "$MESSAGE" $target $action "Installing $emacs_package into ~/.emacs.d"
        local emacs_package_dir=${emacs_package##*/}
        emacs_package_dir=${emacs_package_dir%.git}
        local emacs_package_path=$EMACS_HOME/$emacs_package_dir
        cd $EMACS_HOME
        cd $emacs_package_path && git pull >>$LOG || git clone $emacs_package \
            || printf $FAILURE
    done <<<$emacs_packages
}

function emacs_upgrade_compile {
    local target=$1
    local action=$2

    read -d "" emacs_packages <<EOF
epl
pkg-info.el
goto-chg.el
key-chord.el
s.el
dash.el
emacs-async
exec-path-from-shell
zenburn-emacs
rainbow-delimiters
powerline
spaceline
smartparens
emacs-helm-ag
company-mode
avy
kotlin-mode
js2-mode
web-mode
emmet-mode
dockerfile-mode
plantuml-mode
markdown-mode
yaml-mode
evil-surround
evil-nerd-commenter
EOF

    while read emacs_package; do
        printf "$MESSAGE" $target $action "Compiling $emacs_package in ~/.emacs.d"
        local emacs_package_path=$EMACS_HOME/$emacs_package
        cd $emacs_package_path
        emacs --batch -Q \
        -L $EMACS_HOME/epl \
        -L $EMACS_HOME/pkg-info.el \
        -L $EMACS_HOME/goto-chg.el \
        -L $EMACS_HOME/key-chord.el \
        -L $EMACS_HOME/s.el \
        -L $EMACS_HOME/dash.el \
        -L $EMACS_HOME/emacs-async \
        -L $EMACS_HOME/exec-path-from-shell \
        -L $EMACS_HOME/zenburn-emacs \
        -L $EMACS_HOME/rainbow-delimiters \
        -L $EMACS_HOME/powerline \
        -L $EMACS_HOME/spaceline \
        -L $EMACS_HOME/smartparens \
        -L $EMACS_HOME/helm \
        -L $EMACS_HOME/emacs-helm-ag \
        -L $EMACS_HOME/company-mode \
        -L $EMACS_HOME/avy \
        -L $EMACS_HOME/kotlin-mode \
        -L $EMACS_HOME/js2-mode \
        -L $EMACS_HOME/web-mode \
        -L $EMACS_HOME/emmet-mode \
        -L $EMACS_HOME/idris-mode \
        -L $EMACS_HOME/dockerfile-mode \
        -L $EMACS_HOME/plantuml-mode \
        -L $EMACS_HOME/markdown-mode \
        -L $EMACS_HOME/yaml-mode \
        -L $EMACS_HOME/evil \
        -L $EMACS_HOME/evil-surround \
        -L $EMACS_HOME/evil-nerd-commenter \
        -L $EMACS_HOME/evil-goggles \
        -f batch-byte-compile $emacs_package_path/*.el 2>>$LOG || printf $FAILURE
    done <<<$emacs_packages
}

function emacs_upgrade_make {
    local target=$1
    local action=$2

    read -d "" emacs_packages <<EOF
idris-mode
helm
evil
evil-goggles
EOF

    while read emacs_package; do
        printf "$MESSAGE" $target $action "Making $emacs_package in ~/.emacs.d"
        local emacs_package_path=$EMACS_HOME/$emacs_package
        cd $emacs_package_path
        EMACSLOADPATH=$EMACS_HOME/emacs-async:$EMACS_HOME/evil: make >>$LOG 2>&1 \
            || printf $FAILURE
    done <<<$emacs_packages
}

function emacs_upgrade {
    local target=emacs
    local action=upgrade

    mkdir -p $EMACS_HOME

    emacs_upgrade_web $target $action
    emacs_upgrade_git $target $action
    emacs_upgrade_compile $target $action
    emacs_upgrade_make $target $action
}

function aws_upgrade {
    local target=aws
    local action=upgrade

    printf "$MESSAGE" $target $action "Installing awscli into ~/.local"
    pip install --user awscli >>$LOG || printf $FAILURE
}

rm -f $LOG

read_options $@

# Config action
[[ $ALL_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $COMMON_TARGET ]] && common_config
[[ $ALL_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $GIT_TARGET ]] && git_config
[[ $ALL_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $TMUX_TARGET ]] && tmux_config
[[ $ALL_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $ZSH_TARGET ]] && zsh_config
[[ $ALL_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $ALL_TARGET \
    || $CONFIG_ACTION && $EMACS_TARGET ]] && emacs_config

# Upgrade action
[[ $ALL_ACTION && $ALL_TARGET \
    || $UPGRADE_ACTION && $ALL_TARGET \
    || $UPGRADE_ACTION && $ZSH_TARGET ]] && zsh_upgrade
[[ $ALL_ACTION && $ALL_TARGET \
    || $UPGRADE_ACTION && $ALL_TARGET \
    || $UPGRADE_ACTION && $EMACS_TARGET ]] && emacs_upgrade
[[ $ALL_ACTION && $ALL_TARGET \
    || $UPGRADE_ACTION && $ALL_TARGET \
    || $UPGRADE_ACTION && $AWS_TARGET ]] && aws_upgrade
