#!/bin/sh
#
#  SGIDISPATCH -- Dispatch the SGI transator.

# Initialize the $iraf and environment.
if test -f $HOME/.iraf/envinit ; then
    source $HOME/.iraf/envinit
fi

# Execute the binary
${iraf}/unix/bin.${IRAFARCH}/sgidispatch.e $*
