#!/usr/bin/env bash
export ANSIBLE_NOCOWS=1

read -d '' USAGE << EOM
Usage: ./play option

Options:
    -gc | --git-config
    -ga | --git-all

    -du | --diff-update
    -dc | --diff-config
    -da | --diff-all

    -eu | --emacs-update
    -ec | --emacs-config
    -ea | --emacs-all

    -zu | --zsh-update
    -zc | --zsh-config
    -za | --zsh-all

    -tc | --tmux-config
    -ta | --tmux-all

    -edc | --editorconfig

    -dcu | --docker-compose-update

    -jsu | --js-update
    -jsc | --js-config
    -jsa | --js-all

    -au | --all-update
    -ac | --all-config
    -a  | --all
EOM

ANSIBLE_PLAYBOOK='ansible-playbook -i "localhost," playbook.yml'

case "$1" in

    # git
    -gc|--git-config)
        $ANSIBLE_PLAYBOOK --tags git-config
        ;;
    -ga|--git-all)
        $ANSIBLE_PLAYBOOK --tags git
        ;;

    # diff
    -du|--diff-update)
        $ANSIBLE_PLAYBOOK --tags diff-update
        ;;
    -dc|--diff-config)
        $ANSIBLE_PLAYBOOK --tags diff-config
        ;;
    -da|--diff-all)
        $ANSIBLE_PLAYBOOK --tags diff
        ;;

    # emacs
    -eu|--emacs-update)
        $ANSIBLE_PLAYBOOK --tags emacs-update
        ;;
    -ec|--emacs-config)
        $ANSIBLE_PLAYBOOK --tags emacs-config
        ;;
    -ea|--emacs-all)
        $ANSIBLE_PLAYBOOK --tags emacs
        ;;

    # zsh
    -zu|--zsh-update)
        $ANSIBLE_PLAYBOOK --tags zsh-update
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
    -ta|--tmux-all)
        $ANSIBLE_PLAYBOOK --tags tmux
        ;;

    # editorconfig
    -edc|--editorconfig)
        $ANSIBLE_PLAYBOOK --tags editorconfig
        ;;

    # docker-compose
    -dcu|--docker-compose-update)
        $ANSIBLE_PLAYBOOK --tags docker-compose-update
        ;;

    # js
    -jsu|--js-update)
        $ANSIBLE_PLAYBOOK --tags js-update
        ;;
    -jsc|--js-config)
        $ANSIBLE_PLAYBOOK --tags js-config
        ;;
    -jsa|--js-all)
        $ANSIBLE_PLAYBOOK --tags js
        ;;

    # all
    -au|--all-update)
        $ANSIBLE_PLAYBOOK --tags update
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
        exit -1
        ;;
esac
