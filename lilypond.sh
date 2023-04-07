#!/usr/bin/env zsh

# yay -S fontforge t1utils extractpdfmark pdfcpu
# yay -S dblatex tex-gyre-fonts texlive-langcyrillic

set -eu

LP_HOME=$HOME/.lilypond
LP_URL=https://lilypond.org/download/source/v2.25
LP_VERSION=2.25.3

mkdir -p $LP_HOME && cd $LP_HOME
curl $LP_URL/lilypond-$LP_VERSION.tar.gz | tar -xz
cd lilypond-$LP_VERSION && mkdir -p build && cd build
../autogen.sh --noconfigure \
  && ../configure --prefix=$LP_HOME --disable-documentation \
  GUILE_FLAVOR=guile-3.0
make -j4 all && make install
