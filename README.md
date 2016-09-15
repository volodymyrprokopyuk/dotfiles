# Dotfiles

Config files for:

1. [zsh](http://www.zsh.org/)
1. [tmux](http://tmux.github.io/)
1. [liquidprompt](https://github.com/nojhan/liquidprompt)
1. [emacs](http://www.gnu.org/software/emacs/)
1. [vifm](http://vifm.info/)
1. [tig](https://github.com/jonas/tig)
1. [eslint](http://eslint.org/)
1. [psql](http://www.postgresql.org/docs/9.4/static/app-psql.html)

# Installation

## Install utils and apps

```bash
$ sudo apt-get install xsel
$ sudo apt-get install archivemount
$ sudo apt-get install fuse-zip
$ sudo apt-get install libxml2-utils
$ sudo apt-get install curl
$ sudo apt-get install mercurial
$ sudo apt-get install htop
```

From source:

- [zsh](http://www.zsh.org/)
- [ninja](https://ninja-build.org/)
- [tmux](https://tmux.github.io/)
- [emacs](https://www.gnu.org/software/emacs/)
- [vifm](http://vifm.info/)
- [git](https://git-scm.com/)
- [tig](https://github.com/jonas/tig)
- [ag](https://github.com/ggreer/the_silver_searcher)

## Install zsh

```bash
$ which zsh | sudo tee -a /etc/shells
$ chsh -s $(which zsh)
```

## Install config files

```bash
$ git clone git@github.com:volodymyrprokopyuk/dotfiles.git
$ cd dotfiles
$ ninja
$ ninja update
```
