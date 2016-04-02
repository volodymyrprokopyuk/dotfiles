function updateGit {
  echo [ $1 ];
  cd ~/.emacs.d;
  if cd $1; then git pull; else git clone $2; fi
}

function updateHg {
  echo [ $1 ];
  cd ~/.emacs.d;
  if cd $1; then hg pull -u; else hg clone $2; fi
}

function compileEmacs {
  cd ~/.emacs.d/$1;
  for file in *.el; do
    if [[ -f $file ]]; then
      emacs --batch -Q -L . -f batch-byte-compile $file;
    fi
  done
}

updateGit 'zenburn-emacs' 'https://github.com/bbatsov/zenburn-emacs.git'
  compileEmacs 'zenburn-emacs'
updateGit 'rainbow-delimiters' 'https://github.com/Fanael/rainbow-delimiters.git'
  compileEmacs 'rainbow-delimiters'

updateGit 'emacs-async' 'https://github.com/jwiegley/emacs-async.git'
  compileEmacs 'emacs-async'
updateGit 'helm' 'https://github.com/emacs-helm/helm.git'
  cd ~/.emacs.d/helm && make
updateGit 'yasnippet' '--recursive https://github.com/capitaomorte/yasnippet.git'
  compileEmacs 'yasnippet'

updateGit 'js2-mode' 'https://github.com/mooz/js2-mode.git'
  cd ~/.emacs.d/js2-mode && make
updateGit 'web-mode' 'https://github.com/fxbois/web-mode.git'
  compileEmacs 'web-mode'
updateGit 'emmet-mode' 'https://github.com/smihica/emmet-mode.git'
  compileEmacs 'emmet-mode'
updateGit 'markdown-mode' 'https://github.com/jrblevin/markdown-mode.git'
  compileEmacs 'markdown-mode'
updateGit 'jade-mode' 'https://github.com/brianc/jade-mode.git'
  compileEmacs 'jade-mode'
updateGit 'clojure-mode' 'https://github.com/clojure-emacs/clojure-mode.git'
  compileEmacs 'clojure-mode'
updateGit 'cucumber.el' 'https://github.com/michaelklishin/cucumber.el.git'
  compileEmacs 'cucumber.el'
updateGit 'ninja' 'https://github.com/martine/ninja.git'
  cp ~/.emacs.d/ninja/misc/ninja-mode.el ~/.emacs.d/ninja
  compileEmacs 'ninja'

updateHg 'evil' 'https://bitbucket.org/lyro/evil'
  cd ~/.emacs.d/evil && make
