#!/bin/sh

echo "
source ~/code/conffiles/zshrc
" > ~/.zshrc

ln -s code/conffiles/emacs.d ~/.emacs.d

ln -s code/conffiles/keysnail.js ~/.keysnail.js

