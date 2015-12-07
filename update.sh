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
updateRepo 'smex' 'https://github.com/nonsequitur/smex.git'
updateRepo 'js2-mode' 'https://github.com/mooz/js2-mode.git' \
  && cd ~/.emacs.d/js2-mode && make
updateRepo 'rust-mode' 'https://github.com/rust-lang/rust-mode.git'
updateRepo 'clojure-mode' 'https://github.com/clojure-emacs/clojure-mode.git' \
  && cd ~/.emacs.d/clojure-mode && make
updateRepo 'jade-mode' 'https://github.com/brianc/jade-mode.git' \
  && cd ~/.emacs.d/jade-mode \
  && emacs --batch -f batch-byte-compile jade-mode.el
updateRepo 'markdown-mode' 'https://github.com/defunkt/markdown-mode.git'
updateRepo 'gnuplot-mode' 'https://github.com/mkmcc/gnuplot-mode.git' \
  && cd ~/.emacs.d/gnuplot-mode \
  && emacs --batch -f batch-byte-compile gnuplot-mode.el
updateRepo 'org-mode' 'https://orgmode.org/org-mode.git' \
  && cd ~/.emacs.d/org-mode && make autoloads
updateRepoHG 'evil' 'https://bitbucket.org/lyro/evil' \
  && cd ~/.emacs.d/evil && make
