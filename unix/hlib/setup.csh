# Allow a previously defined $iraf to be used.
set d_iraf = /iraf/iraf/
if ( ! ( $?iraf )) then
    if ( -r ${HOME}/.iraf/irafroot ) then
	setenv iraf `cat /etc/iraf/irafroot`
    else if ( -r /etc/iraf/irafroot ) then
	setenv iraf `cat /etc/iraf/irafroot`
    else
	setenv iraf $d_iraf
    endif
endif

# Allow a previously defined $IRAFARCH to be used.
if ( ! ( $?IRAFARCH )) then
    setenv IRAFARCH	`$iraf/unix/hlib/irafarch.sh -actual`
endif

set path = ($HOME/.iraf/bin $path)
set cdpath  = ($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot)

# The world'd most obvious alias ....
alias iraf	"xgterm -e cl &"

rehash
