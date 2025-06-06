#!/bin/csh -f

# Allow a previously defined $iraf to be used.
if ( ! ( $?iraf )) then
    setenv iraf	/opt/iraf/iraf/
endif

# Allow a previously defined $IRAFARCH to be used.
if ( ! ( $?IRAFARCH )) then
    if ( -e ${iraf}/unix/hlib/irafarch.csh ) then
        setenv IRAFARCH	`${iraf}/unix/hlib/irafarch.csh -actual`
    endif
endif

# Define a minimal IRAF runtime environment.
setenv  hostid  unix
setenv  host    ${iraf}/unix/
setenv  hlib    ${iraf}/unix/hlib/
setenv  hbin    ${iraf}/unix/bin.${IRAFARCH}/
setenv  tmp     /tmp/

# Uncomment to define a full IRAF development environment.
#if ( -e $iraf/unix/hlib/irafuser.csh ) then
#    source $iraf/unix/hlib/irafuser.csh
#endif

set path = ($HOME/.iraf/bin $path)
set cdpath  = ($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot)

# The world's most obvious alias ....
alias iraf	"xgterm -e cl &"

rehash

