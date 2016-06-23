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
      emacs --batch -Q -L . -L ../helm -f batch-byte-compile $FILE
    fi
  done
}

BASE_DIR=$HOME/local/bin

echo [ 'liquidprompt' ]
GIT_URL='https://github.com/nojhan/liquidprompt.git'
updateGit $BASE_DIR $GIT_URL
echo [ 'git-completion' ]
GIT_URL='https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash'
curl $GIT_URL -o $BASE_DIR/git-completion.bash
echo [ 'ninja-completion' ]
GIT_URL='https://raw.githubusercontent.com/ninja-build/ninja/master/misc/bash-completion'
curl $GIT_URL -o $BASE_DIR/ninja-completion.bash
echo [ 'kerl' ]
GIT_URL='https://raw.githubusercontent.com/kerl/kerl/master/kerl'
curl $GIT_URL -o $BASE_DIR/kerl && chmod 755 $BASE_DIR/kerl
GIT_URL='https://raw.githubusercontent.com/kerl/kerl/master/bash_completion/kerl'
curl $GIT_URL -o $BASE_DIR/kerl-completion.bash

BASE_DIR=$HOME/.emacs.d

echo [ 'zenburn' ]
GIT_URL='https://github.com/bbatsov/zenburn-emacs.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
echo [ 'rainbow-delimiters' ]
GIT_URL='https://github.com/Fanael/rainbow-delimiters.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

echo [ 'emacs-async' ]
GIT_URL='https://github.com/jwiegley/emacs-async.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
echo [ 'helm' ]
GIT_URL='https://github.com/emacs-helm/helm.git'
updateGit $BASE_DIR $GIT_URL && make
echo [ 'helm-ag' ]
GIT_URL='https://github.com/syohex/emacs-helm-ag.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
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
echo [ 'clojure-mode' ]
GIT_URL='https://github.com/clojure-emacs/clojure-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
echo [ 'cucumber-mode' ]
GIT_URL='https://github.com/michaelklishin/cucumber.el.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
echo [ 'ninja-mode' ]
GIT_URL='https://raw.githubusercontent.com/ninja-build/ninja/master/misc/ninja-mode.el'
mkdir -p $BASE_DIR/ninja && curl $GIT_URL -o $BASE_DIR/ninja/ninja-mode.el
GIT_URL='https://github.com/ninja-build/ninja.git'
compileEmacs $BASE_DIR $GIT_URL

echo [ 'evil' ]
GIT_URL='https://bitbucket.org/lyro/evil'
updateHg $BASE_DIR $GIT_URL && make
