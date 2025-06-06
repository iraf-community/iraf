#!/bin/sh
#
#  SGIDEBUG -- SGI translator debug utility.

# Initialize the $iraf and environment.
if test -f $HOME/.iraf/envinit ; then
    source $HOME/.iraf/envinit
fi

# Execute the binary
${iraf}/unix/bin.${IRAFARCH}/sgidebug.e $*
