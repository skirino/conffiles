#!/bin/sh

echo "
. $HOME/code/conffiles/zsh/zprofile
" >> ~/.profile

echo "
. $HOME/code/conffiles/zsh/zshrc
" > ~/.zshrc

ln -s ~/code/conffiles/emacs.d ~/.emacs.d
ln -s ~/code/conffiles/vimrc ~/.vimrc
mkdir -p ~/.xmonad
ln -s ~/code/conffiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s ~/code/conffiles/xmonad/xmobarrc ~/.xmobarrc
ln -s ~/code/conffiles/xbindkeysrc ~/.xbindkeysrc
mkdir -p ~/.percol.d
ln -s ~/code/conffiles/percol/rc.py ~/.percol.d/rc.py
mkdir -p ~/.config/gtk-3.0
ln -s ~/code/conffiles/gtk/settings.ini ~/.config/gtk-3.0/settings.ini
mkdir -p ~/.themes/Emacs/gtk-3.0
ln -s ~/code/conffiles/gtk/gtk-keys.css ~/.themes/Emacs/gtk-3.0/gtk-keys.css
