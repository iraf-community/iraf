#!/bin/csh -f

# Allow a previously defined $iraf to be used.
if ( ! ( $?iraf )) then
    setenv iraf	/Applications/IRAF.app/Contents/iraf-v218/
endif

# Allow a previously defined $IRAFARCH to be used.
if ( ! ( $?IRAFARCH )) then
    if ( -e $iraf/unix/hlib/irafarch.csh ) then
        setenv IRAFARCH	`$iraf/unix/hlib/irafarch.csh -actual`
    endif
endif

# Uncomment to define an IRAF development environment.
#if ( -e $iraf/unix/hlib/irafuser.csh ) then
#    source $iraf/unix/hlib/irafuser.csh
#endif

set path = ($HOME/.iraf/bin $path)
set cdpath  = ($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot)

# The world's most obvious alias ....
alias iraf	"xgterm -e cl &"

rehash

