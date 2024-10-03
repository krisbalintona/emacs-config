#!/bin/sh

# * Load everything from .xprofile
source "/home/krisbalintona/.xprofile"

# * Start Emacs
exec dbus-launch --exit-with-session emacs -mm --debug-init
#-l ~/.emacs.d/desktop.el
