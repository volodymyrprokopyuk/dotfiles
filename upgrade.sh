#!/usr/bin/env bash

export ANSIBLE_NOCOWS=1

read -d "" USAGE << EOM
Usage: ./play option

Options:
    -gc | --git-config

    -dc | --diff-config

    -tc | --tmux-config

    -cc | --common-config

    -zu | --zsh-upgrade
    -zc | --zsh-config
    -za | --zsh-all

    -eu | --emacs-upgrade
    -ec | --emacs-config
    -ea | --emacs-all

    -awsu | --aws-upgrade

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

    # tmux
    -tc|--tmux-config)
        $ANSIBLE_PLAYBOOK --tags tmux-config
        ;;

    # editor-config
    -cc|--common-config)
        $ANSIBLE_PLAYBOOK --tags common-config
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

    # aws-upgrade
    -awsu|--aws-upgrade)
        $ANSIBLE_PLAYBOOK --tags aws-upgrade
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
