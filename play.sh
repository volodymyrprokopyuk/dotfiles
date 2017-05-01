#!/usr/bin/env bash
export ANSIBLE_NOCOWS=1

read -d '' USAGE << EOM
Usage: ./play option

Options:
  -gc | --git-config
  -g  | --git

  -eu | --emacs-update
  -ec | --emacs-config
  -e  | --emacs

  -zu | --zsh-update
  -zc | --zsh-config
  -z  | --zsh

  -tc | --tmux-config
  -t  | --tmux

  -au | --all-update
  -ac | --all-config
  -a  | -all
EOM

ANSIBLE_PLAYBOOK='ansible-playbook -i "localhost," playbook.yml'

case $1 in

  # git
  -gc|--git-config)
    $ANSIBLE_PLAYBOOK --tags git-config
    ;;
  -g|--git)
    $ANSIBLE_PLAYBOOK --tags git
    ;;

  # emacs
  -eu|--emacs-update)
    $ANSIBLE_PLAYBOOK --tags emacs-update
    ;;
  -ec|--emacs-config)
    $ANSIBLE_PLAYBOOK --tags emacs-config
    ;;
  -e|--emacs)
    $ANSIBLE_PLAYBOOK --tags emacs
    ;;

  # zsh
  -zu|--zsh-update)
    $ANSIBLE_PLAYBOOK --tags zsh-update
    ;;
  -zc|--zsh-config)
    $ANSIBLE_PLAYBOOK --tags zsh-config
    ;;
  -z|--zsh)
    $ANSIBLE_PLAYBOOK --tags zsh
    ;;

  # tmux
  -tc|--tmux-config)
    $ANSIBLE_PLAYBOOK --tags tmux-config
    ;;
  -t|--tmux)
    $ANSIBLE_PLAYBOOK --tags tmux
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
    echo "$USAGE"
    exit -1
    ;;
esac
