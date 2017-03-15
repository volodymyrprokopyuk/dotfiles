function updateGit {
  DIR=${2##*/}; DIR=${DIR%.*}
  mkdir -p $1 && cd $1
  if cd $DIR; then git pull; else git clone $2 && cd $DIR; fi
}

function updateGitWithReset {
  DIR=${2##*/}; DIR=${DIR%.*}
  mkdir -p $1 && cd $1
  if cd $DIR; then git reset --hard && git pull; else git clone $2 && cd $DIR; fi
}

function updateHg {
  DIR=${2##*/}; DIR=${DIR%.*}
  mkdir -p $1 && cd $1
  if cd $DIR; then hg pull -u; else hg clone $2 && cd $DIR; fi
}

function compileEmacs {
  DIR=${2##*/}; DIR=${DIR%.*}
  cd $1/$DIR
  for FILE in *.el; do
    if [[ -f $FILE ]]; then
      emacs --batch -Q -L . -L ../dash.el -L ../helm -f batch-byte-compile $FILE
    fi
  done
}

echo [ 'git-aliases' ]
git config --global user.name "Volodymyr Prokopyuk"
git config --global user.email "volodymyrprokopyuk@gmail.com"
git config --global core.editor "em -nw"
git config --global core.pager "less -r"
git config --global alias.cl "clone"
git config --global alias.l "log --all --color --graph --abbrev-commit --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
git config --global alias.s "status -sb"
git config --global alias.d "diff --color-words"
git config --global alias.dc "diff --color-words --cached"
git config --global alias.a "!git add -A && git s"
git config --global alias.cm "commit"
git config --global alias.ch "checkout"
git config --global alias.chb "checkout -b"
git config --global alias.bs "branch -a -vv"
git config --global alias.rs "remote -v"

BASE_DIR=$HOME/.zsh

echo [ 'liquidprompt' ]
GIT_URL='https://github.com/nojhan/liquidprompt.git'
updateGit $BASE_DIR $GIT_URL

echo [ 'zsh-completions' ]
GIT_URL='https://github.com/zsh-users/zsh-completions.git'
updateGit $BASE_DIR $GIT_URL

echo [ 'zsh-autosuggestions' ]
GIT_URL='https://github.com/zsh-users/zsh-autosuggestions.git'
updateGit $BASE_DIR $GIT_URL

echo [ 'zsh-syntax-highlighting' ]
GIT_URL='https://github.com/zsh-users/zsh-syntax-highlighting.git'
updateGit $BASE_DIR $GIT_URL

BASE_DIR=$HOME/.zsh/completions

echo [ 'ninja-completion' ]
GIT_URL='https://raw.githubusercontent.com/ninja-build/ninja/master/misc/zsh-completion'
curl $GIT_URL -o $BASE_DIR/_ninja

echo [ 'kerl-completion' ]
GIT_URL='https://raw.githubusercontent.com/kerl/kerl/master/zsh_completion/_kerl'
curl $GIT_URL -o $BASE_DIR/_kerl

BASE_DIR=$HOME/local/bin

echo [ 'kerl' ]
GIT_URL='https://raw.githubusercontent.com/kerl/kerl/master/kerl'
curl $GIT_URL -o $BASE_DIR/kerl && chmod 755 $BASE_DIR/kerl

BASE_DIR=$HOME/.emacs.d

echo [ 'dash' ]
GIT_URL='https://github.com/magnars/dash.el.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'emacs-async' ]
GIT_URL='https://github.com/jwiegley/emacs-async.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'zenburn' ]
GIT_URL='https://github.com/bbatsov/zenburn-emacs.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'spacemacs-theme' ]
GIT_URL='https://github.com/nashamri/spacemacs-theme.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'rainbow-delimiters' ]
GIT_URL='https://github.com/Fanael/rainbow-delimiters.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'powerline' ]
GIT_URL='https://github.com/milkypostman/powerline.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'neotree' ]
GIT_URL='https://github.com/jaypei/emacs-neotree.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'ivy' ]
GIT_URL='https://github.com/abo-abo/swiper.git'
updateGit $BASE_DIR $GIT_URL && make

#echo [ 'helm' ]
#GIT_URL='https://github.com/emacs-helm/helm.git'
#updateGit $BASE_DIR $GIT_URL && make
#
#echo [ 'helm-ag' ]
#GIT_URL='https://github.com/syohex/emacs-helm-ag.git'
#updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'yasnippet' ]
GIT_URL='https://github.com/capitaomorte/yasnippet.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'js2-mode' ]
GIT_URL='https://github.com/mooz/js2-mode.git'
updateGit $BASE_DIR $GIT_URL && make

echo [ 'web-mode' ]
GIT_URL='https://github.com/fxbois/web-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'emmet-mode' ]
GIT_URL='https://github.com/smihica/emmet-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'markdown-mode' ]
GIT_URL='https://github.com/jrblevin/markdown-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'jade-mode' ]
GIT_URL='https://github.com/brianc/jade-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'elixir-mode' ]
GIT_URL='https://github.com/elixir-lang/emacs-elixir.git'
updateGitWithReset $BASE_DIR $GIT_URL \
  && sed -i -e "/^(require 'pkg-info).*/d" \
    -e "s/(pkg-info-version-info 'elixir-mode)/\"VERSION\"/g" \
    elixir-mode.el \
  && compileEmacs $BASE_DIR $GIT_URL

echo [ 'cucumber-mode' ]
GIT_URL='https://github.com/michaelklishin/cucumber.el.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'ninja-mode' ]
GIT_URL='https://raw.githubusercontent.com/ninja-build/ninja/master/misc/ninja-mode.el'
mkdir -p $BASE_DIR/ninja && curl $GIT_URL -o $BASE_DIR/ninja/ninja-mode.el
GIT_URL='https://github.com/ninja-build/ninja.git'
compileEmacs $BASE_DIR $GIT_URL

echo [ 'goto-chg' ]
GIT_URL='https://www.emacswiki.org/emacs/download/goto-chg.el'
mkdir -p $BASE_DIR/goto-chg && curl $GIT_URL -o $BASE_DIR/goto-chg/goto-chg.el
compileEmacs $BASE_DIR $GIT_URL

echo [ 'evil' ]
GIT_URL='https://github.com/emacs-evil/evil'
updateGit $BASE_DIR $GIT_URL && make
