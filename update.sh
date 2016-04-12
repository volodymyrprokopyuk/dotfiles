function updateGit {
  DIR=${2##*/}; DIR=${DIR%.*}
  echo [ $DIR ]
  mkdir -p $1 && cd $1
  if cd $DIR; then git pull; else git clone $2; fi
}

function updateHg {
  DIR=${2##*/}; DIR=${DIR%.*}
  echo [ $DIR ]
  mkdir -p $1 && cd $1
  if cd $DIR; then hg pull -u; else hg clone $2; fi
}

function compileEmacs {
  DIR=${2##*/}; DIR=${DIR%.*}
  cd "$1/$DIR"
  for FILE in *.el; do
    if [[ -f $FILE ]]; then
      emacs --batch -Q -L . -f batch-byte-compile $FILE
    fi
  done
}

BASE_DIR="$HOME/local/bin"

GIT_URL='https://github.com/nojhan/liquidprompt.git'
updateGit $BASE_DIR $GIT_URL

GIT_URL='https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash'
echo [ 'git-completion' ]
curl $GIT_URL -o "$BASE_DIR/git-completion.bash"

BASE_DIR="$HOME/.emacs.d"

GIT_URL='https://github.com/bbatsov/zenburn-emacs.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/Fanael/rainbow-delimiters.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

GIT_URL='https://github.com/jwiegley/emacs-async.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/emacs-helm/helm.git'
updateGit $BASE_DIR $GIT_URL && cd "$BASE_DIR/helm" && make
GIT_URL='https://github.com/capitaomorte/yasnippet.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL

GIT_URL='https://github.com/mooz/js2-mode.git'
updateGit $BASE_DIR $GIT_URL && cd "$BASE_DIR/js2-mode" && make
GIT_URL='https://github.com/fxbois/web-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/smihica/emmet-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/jrblevin/markdown-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/brianc/jade-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/clojure-emacs/clojure-mode.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/michaelklishin/cucumber.el.git'
updateGit $BASE_DIR $GIT_URL && compileEmacs $BASE_DIR $GIT_URL
GIT_URL='https://github.com/martine/ninja.git'
updateGit $BASE_DIR $GIT_URL \
  && cp "$BASE_DIR/ninja/misc/ninja-mode.el" "$BASE_DIR/ninja" \
  && compileEmacs $BASE_DIR $GIT_URL

GIT_URL='https://bitbucket.org/lyro/evil'
updateHg $BASE_DIR $GIT_URL && cd "$BASE_DIR/evil" && make
