#!/bin/csh
# IRAF cshell login file.  [SITE DEPENDENT]

setenv IRAFARCH linux
source ~/.cshrc

set cdpath = (~ $iraf $iraf/unix $iraf/unix/boot $iraf/sys $iraf/pkg $iraf/noao\
    /iraf /iraf/extern /iraf/x11iraf)

if (`tty` == "/dev/tty1") then
    echo "starting window system in 5 seconds (ctrl/c to abort)..."
    sleep 5; xinit
endif
