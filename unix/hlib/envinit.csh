#!/bin/csh
#
# Initialize the /opt/iraf/iraf/ path and environment.

source $iraf/unix/hlib/setup.csh
if ( -e $iraf/unix/hlib/irafuser.csh ) then
    source $iraf/unix/hlib/irafuser.csh
endif
