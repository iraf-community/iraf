# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SSYSTEM -- Make the named world coordinate system the default.

procedure mw_ssystem (mw, system)

pointer	mw			#I pointer to MWCS descriptor
char	system[ARB]		#I system name

pointer	wp
bool	streq()
pointer	mw_findsys()
errchk	mw_findsys

begin
	if (streq (system, "world"))
	    call mw_sdefwcs (mw)		# set default world system
	else {
	    wp = mw_findsys (mw, system)
	    if (wp != NULL)
		MI_WCS(mw) = wp
	    else
		call syserrs (SYS_MWWCSNF, system)
	}
end
