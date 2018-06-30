#!/usr/bin/env bash

export ANSIBLE_NOCOWS=1

read -d "" USAGE << EOM
Usage: ./play option

Options:
    -gc | --git-config

    -dc | --diff-config

    -eu | --emacs-upgrade
    -ec | --emacs-config
    -ea | --emacs-all

    -zu | --zsh-upgrade
    -zc | --zsh-config
    -za | --zsh-all

    -tc | --tmux-config

    -edc | --editor-config

    -au | --all-upgrade
    -ac | --all-config
    -a  | --all
EOM

ANSIBLE_PLAYBOOK='ansible-playbook -i "localhost," upgrade.yml'

case "$1" in

    # git
    -gc|--git-config)
        $ANSIBLE_PLAYBOOK --tags git-config
        ;;

    # diff
    -dc|--diff-config)
        $ANSIBLE_PLAYBOOK --tags diff-config
        ;;

    # emacs
    -eu|--emacs-upgrade)
        $ANSIBLE_PLAYBOOK --tags emacs-upgrade
        ;;
    -ec|--emacs-config)
        $ANSIBLE_PLAYBOOK --tags emacs-config
        ;;
    -ea|--emacs-all)
        $ANSIBLE_PLAYBOOK --tags emacs
        ;;

    # zsh
    -zu|--zsh-upgrade)
        $ANSIBLE_PLAYBOOK --tags zsh-upgrade
        ;;
    -zc|--zsh-config)
        $ANSIBLE_PLAYBOOK --tags zsh-config
        ;;
    -za|--zsh-all)
        $ANSIBLE_PLAYBOOK --tags zsh
        ;;

    # tmux
    -tc|--tmux-config)
        $ANSIBLE_PLAYBOOK --tags tmux-config
        ;;

    # editor-config
    -edc|--editor-config)
        $ANSIBLE_PLAYBOOK --tags editor-config
        ;;

    # all
    -au|--all-upgrade)
        $ANSIBLE_PLAYBOOK --tags upgrade
        ;;
    -ac|--all-config)
        $ANSIBLE_PLAYBOOK --tags config
        ;;
    -a|--all)
        $ANSIBLE_PLAYBOOK
        ;;

    # usage
    *)
        echo "${USAGE}"
        exit 1
        ;;
esac
