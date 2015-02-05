# coldnew's spacemacs config

[![Build Status](https://travis-ci.org/coldnew/coldnew-spacemacs.svg?branch=master)](https://travis-ci.org/coldnew/coldnew-spacemacs)

This is my [spacemacs](https://github.com/syl20bnr/spacemacs) config, most of them is based on [coldnew-emacs](https://github.com/coldnew/coldnew-emacs) but
use English instead.

Feal free to use it :).

## Install or testing this config

-   First use git to download whole repo

        git clone https://github.com/coldnew/coldnew-spacemacs.git

-   Then use git submodule to download the spacemacs

        git submodule init
        git submodule update

-   If you not put this repo on `~/.emacs.d`, you need to use following
    command to start emacs

        emacs -q -l ~/coldnew-spacemacs/init.el

## Packages need to install to system

Some extra packages need to install in system manually.

- Mac OSX

        brew install the_silver_searcher
        brew install fasd
        brew install mu --with-emacs --HEAD

- Gentoo Linux

        emerge the_silver_searcher
        emerge fasd
        USE="emacs" emerge net-mail/mu
