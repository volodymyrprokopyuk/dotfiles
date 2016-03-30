function updateRepo {
  echo [ $1 ];
  cd ~/.emacs.d;
  if cd $1; then git pull; else git clone $2; fi
}

function updateRepoHG {
  echo [ $1 ];
  cd ~/.emacs.d;
  if cd $1; then hg pull -u; else hg clone $2; fi
}

#cd ~/.emacs.d && emacs --batch -f batch-byte-compile emacs.el

updateRepo 'zenburn-emacs' 'https://github.com/bbatsov/zenburn-emacs.git'
updateRepo 'rainbow-delimiters' 'https://github.com/Fanael/rainbow-delimiters.git'
updateRepo 'smex' 'https://github.com/nonsequitur/smex.git'
updateRepo 'emacs-async' 'https://github.com/jwiegley/emacs-async.git' \
  && cd ~/.emacs.d/emacs-async \
  && emacs --batch -f batch-byte-compile async.el
updateRepo 'helm' 'https://github.com/emacs-helm/helm.git' \
  && cd ~/.emacs.d/helm && make

updateRepo 'ninja' 'https://github.com/martine/ninja.git' \
  && cd ~/.emacs.d/ninja && cp misc/ninja-mode.el . \
  && emacs --batch -f batch-byte-compile ninja-mode.el
updateRepo 'js2-mode' 'https://github.com/mooz/js2-mode.git' \
  && cd ~/.emacs.d/js2-mode && make
updateRepo 'web-mode' 'https://github.com/fxbois/web-mode.git' \
  && cd ~/.emacs.d/web-mode \
  && emacs --batch -f batch-byte-compile web-mode.el
updateRepo 'emmet-mode' 'https://github.com/smihica/emmet-mode.git' \
  && cd ~/.emacs.d/emmet-mode \
  && emacs --batch -f batch-byte-compile emmet-mode.el
updateRepo 'jade-mode' 'https://github.com/brianc/jade-mode.git' \
  && cd ~/.emacs.d/jade-mode \
  && emacs --batch -f batch-byte-compile jade-mode.el
updateRepo 'markdown-mode' 'https://github.com/defunkt/markdown-mode.git' \
  && cd ~/.emacs.d/markdown-mode \
  && emacs --batch -f batch-byte-compile markdown-mode.el
updateRepo 'cucumber.el' 'https://github.com/michaelklishin/cucumber.el.git' \
  && cd ~/.emacs.d/cucumber.el \
  && emacs --batch -f batch-byte-compile feature-mode.el
updateRepo 'clojure-mode' 'https://github.com/clojure-emacs/clojure-mode.git' \
  && cd ~/.emacs.d/clojure-mode && make

updateRepo 'yasnippet' '--recursive https://github.com/capitaomorte/yasnippet.git' \
  && cd ~/.emacs.d/yasnippet \
  && emacs --batch -f batch-byte-compile yasnippet.el
updateRepoHG 'evil' 'https://bitbucket.org/lyro/evil' \
  && cd ~/.emacs.d/evil && make
