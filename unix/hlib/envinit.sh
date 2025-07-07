#!/bin/sh
#
# Initialize the /opt/iraf/iraf/ path and environment.

source $iraf/unix/hlib/setup.sh
if test -f $iraf/unix/hlib/irafuser.sh ; then
    source $iraf/unix/hlib/irafuser.sh
fi
