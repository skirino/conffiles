#!/bin/sh

echo "
. $HOME/code/conffiles/zsh/zprofile
" >> ~/.profile

echo "
. $HOME/code/conffiles/zsh/zshrc
" > ~/.zshrc

ln -s code/conffiles/emacs.d ~/.emacs.d

ln -s code/conffiles/keysnail.js ~/.keysnail.js

