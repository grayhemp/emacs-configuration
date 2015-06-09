# Emacs configuration

A set of Emacs configuration scripts, Org mode customizations and many
more for software development and organizing.

## Installation

Clone the repository.

    git clone https://github.com/grayhemp/emacs-configuration.git

Backup and remove original `~/.emacs.d` directory and create a symlink
to the cloned repository instead.

    ln -s emacs-configuration/ ~/.emacs.d

Instantiate the organizing templates.

    cd emacs-configuration
    cp organizing_skel organizing

Create an empty custom configuration file.

    touch custom.el

Install necessary packages. Usualy they are mmm-mode, haskell-mode,
but, depending on your distribution, Emacs might ask for other ones
when started. Then, finally, in Emacs, open `organizing.org` and
`routine.org` files from the `emacs-configuration/organizing`
directory and do `C-c [` in each of them to add to agenda.

## Getting started

To walk through the features start with reviewing
[init.el](init.el). The code is well commented and structured, so it
should be easy. Also read [the organizing system
description](organizing_skel/organizing.txt) to get an insight of it
and, if you are interested in words learning, [the Pyramidal technique
description](organizing_skel/pyramidal.txt).

## Authors

- [Sergey Konoplev](mailto:gray.ru@gmail.com)
