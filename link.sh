#!/bin/bash

# the absolute path in which our dotfiles reside
dotfiles_dir=$(dirname $(readlink -f $0))

for file in .bashrc .emacs.d .i3 .i3status.conf .xinitrc .Xresources; do
    ln -s $dotfiles_dir/$file $HOME/$file
done
