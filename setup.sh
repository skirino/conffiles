#!/usr/bin/env bash
set -euo pipefail -o posix

confdir=$(dirname $(realpath "$0"))

(
  cd "${confdir}"
  git submodule update --init
)

echo "
. ${confdir}/zsh/zshrc
" >> ~/.zshrc

ln -s "${confdir}/emacs.d" ~/.emacs.d
emacs -nw --script ~/.emacs.d/init.el
