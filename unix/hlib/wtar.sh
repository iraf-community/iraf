#!/bin/sh
#
#  WTAR -- Write a tar file.

# Initialize the $iraf and environment.
if test -f $HOME/.iraf/envinit ; then
    source $HOME/.iraf/envinit
fi

# Execute the binary
${iraf}/unix/bin.${IRAFARCH}/wtar.e $*
