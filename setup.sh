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

mkdir -p ~/bin
curl -L https://github.com/skirino/direnv_rust/releases/download/0.1.0/direnv_rust_linux_amd64 --output ~/bin/direnv_rust
chmod +x ~/bin/direnv_rust

ln -s "${confdir}/emacs.d" ~/.emacs.d
emacs -nw --script ~/.emacs.d/init.el
