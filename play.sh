#!/usr/bin/env bash

export ANSIBLE_NOCOWS=1

read -d '' USAGE << EOM
./play
  -gc | --git-config
  -g  | --git

  -eu | --emacs-update
  -ec | --emacs-config
  -e  | --emacs

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
