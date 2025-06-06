#!/bin/sh
#
#  GENERIC -- Compile a generic source.

# Initialize the $iraf and environment.
if test -f $HOME/.iraf/envinit ; then
    source $HOME/.iraf/envinit
fi

# Execute the binary
${iraf}/unix/bin.${IRAFARCH}/generic.e $*
