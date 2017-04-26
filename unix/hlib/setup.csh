#!/bin/csh -f

# Allow a previously defined $iraf to be used.
if ( ! ( $?iraf )) then
    setenv iraf	/iraf/iraf/
endif

# Allow a previously defined $IRAFARCH to be used.
if ( ! ( $?IRAFARCH )) then
    setenv IRAFARCH	`$iraf/unix/hlib/irafarch.csh -actual`
endif
source $iraf/unix/hlib/irafuser.csh

set path = ($HOME/.iraf/bin $path)
set cdpath  = ($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot)

# The world'd most obvious alias ....
alias iraf	"xgterm -e cl &"

rehash

