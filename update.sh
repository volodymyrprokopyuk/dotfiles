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
updateRepo 'zenburn-emacs' 'https://github.com/bbatsov/zenburn-emacs.git'
updateRepo 'rainbow-delimiters' 'https://github.com/Fanael/rainbow-delimiters.git'
updateRepo 'js2-mode' 'https://github.com/mooz/js2-mode.git' \
  && cd ~/.emacs.d/js2-mode && make
updateRepo 'tuareg' 'https://github.com/ocaml/tuareg.git' \
  && cd ~/.emacs.d/tuareg && make
updateRepo 'haskell-mode' 'https://github.com/haskell/haskell-mode.git' \
  && cd ~/.emacs.d/haskell-mode && EMACS=/usr/local/bin/emacs make
updateRepo 'org-mode' 'https://orgmode.org/org-mode.git' \
  && cd ~/.emacs.d/org-mode && make autoloads
updateRepo 'jade-mode' 'https://github.com/brianc/jade-mode.git'
updateRepo 'markdown-mode' 'https://github.com/defunkt/markdown-mode.git'
updateRepo 'gnuplot-mode' 'https://github.com/mkmcc/gnuplot-mode.git'
updateRepo 'dash' 'https://github.com/magnars/dash.el.git dash'
updateRepo 'magit' 'https://github.com/magit/magit.git' \
  && cd ~/.emacs.d/magit && make
updateRepoHG 'evil' 'https://bitbucket.org/lyro/evil' \
  && cd ~/.emacs.d/evil && make
