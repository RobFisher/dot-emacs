dot-emacs
=========

My Emacs configuration.

Install
-------
sudo apt-get install emacs24 emacs24-el


Set up Font
-----------

Edit ~/.Xresources and add:

    emacs24.font: 6x13


.emacs file
-----------

    cd
    git clone https://github.com/RobFisher/dot-emacs.git
    ln -s dot-emacs/.emacs .emacs
    mkdir -p .emacs.d/lisp
    scp ukltfisherr:~/.emacs.d/lisp/* .emacs.d/lisp/

GNU Global
----------

    cd Download
    wget http://tamacom.com/global/global-6.2.10.tar.gz
    tar -zxvf global-6.2.10.tar.gz
    cd global-6.2.10
    sudo apt-get install libncurses5-dev
    ./configure
    make
    sudo make install

    cd ~/.emacs.d/lisp
    wget http://www.emacswiki.org/emacs/xgtags.el


Autocomplete
------------

    wget http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2
    tar -jxvf auto-complete-1.3.1.tar.bz2
    cd auto-complete-1.3.1/
    make
    sudo cp -R * /usr/share/emacs24/site-lisp/


SMEX
----

SMEX enhances the M-x interface.

    cd ~/.emacs.d/
    git clone https://github.com/nonsequitur/smex.git
    cd lisp
    ln -s ../smex/smex.el smex.el

Solarized theme
---------------

    cd ~/.emacs.d/
    git clone https://github.com/sellout/emacs-color-theme-solarized.git
      
Then use M-x customize-variable custom-theme-load-path and add:

    ~/.emacs.d/emacs-color-theme-solarized.

Make sure (load-theme 'solarized-dark t) is at the end of .emacs .


gccrec
------

This is needed for using Flymake mode when make cannot be used.

    sudo apt-get install ruby ruby-sqlite3
    cd ~/Downloads
    wget http://cx4a.org/pub/gccsense/gccsense-0.1.tar.bz2
    tar -jxvf gccsense-0.1.tar.bz2
    sudo cp bin/gccrec /usr/bin/
