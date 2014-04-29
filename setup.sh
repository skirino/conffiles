#!/bin/sh

echo "
. $HOME/code/conffiles/zsh/zprofile
" >> ~/.profile

echo "
. $HOME/code/conffiles/zsh/zshrc
" > ~/.zshrc

ln -s ~/code/conffiles/emacs.d     ~/.emacs.d
ln -s ~/code/conffiles/keysnail.js ~/.keysnail.js
mkdir -p ~/.xmonad
ln -s ~/code/conffiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s ~/code/conffiles/xmonad/xmobarrc ~/.xmobarrc
mkdir -p ~/.percol.d
ln -s ~/code/conffiles/percol/rc.py ~/.percol.d/rc.py
