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
    if [[ -f $file ]] && [[ ! $file =~ test ]]; then
      emacs --batch -f batch-byte-compile $file;
    fi
  done
}

#updateGit 'zenburn-emacs' 'https://github.com/bbatsov/zenburn-emacs.git'
#  compileEmacs 'zenburn-emacs'
#updateGit 'rainbow-delimiters' 'https://github.com/Fanael/rainbow-delimiters.git'
#  compileEmacs 'rainbow-delimiters'

#updateGit 'js2-mode' 'https://github.com/mooz/js2-mode.git'
#  cd ~/.emacs.d/js2-mode && make
#updateGit 'web-mode' 'https://github.com/fxbois/web-mode.git'
#  compileEmacs 'web-mode'
#updateGit 'emmet-mode' 'https://github.com/smihica/emmet-mode.git'
#  compileEmacs 'emmet-mode'

# updateGit 'smex' 'https://github.com/nonsequitur/smex.git'
# updateGit 'emacs-async' 'https://github.com/jwiegley/emacs-async.git' \
#   && cd ~/.emacs.d/emacs-async \
#   && emacs --batch -f batch-byte-compile async.el
# updateGit 'helm' 'https://github.com/emacs-helm/helm.git' \
#   && cd ~/.emacs.d/helm && make
#
# updateGit 'ninja' 'https://github.com/martine/ninja.git' \
#   && cd ~/.emacs.d/ninja && cp misc/ninja-mode.el . \
#   && emacs --batch -f batch-byte-compile ninja-mode.el
# updateGit 'jade-mode' 'https://github.com/brianc/jade-mode.git' \
#   && cd ~/.emacs.d/jade-mode \
#   && emacs --batch -f batch-byte-compile jade-mode.el
# updateGit 'markdown-mode' 'https://github.com/defunkt/markdown-mode.git' \
#   && cd ~/.emacs.d/markdown-mode \
#   && emacs --batch -f batch-byte-compile markdown-mode.el
# updateGit 'cucumber.el' 'https://github.com/michaelklishin/cucumber.el.git' \
#   && cd ~/.emacs.d/cucumber.el \
#   && emacs --batch -f batch-byte-compile feature-mode.el
# updateGit 'clojure-mode' 'https://github.com/clojure-emacs/clojure-mode.git' \
#   && cd ~/.emacs.d/clojure-mode && make
#
# updateGit 'yasnippet' '--recursive https://github.com/capitaomorte/yasnippet.git' \
#   && cd ~/.emacs.d/yasnippet \
#   && emacs --batch -f batch-byte-compile yasnippet.el

#updateHg 'evil' 'https://bitbucket.org/lyro/evil'
#  cd ~/.emacs.d/evil && make
