# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plio.h>

# PLVALID -- Verify the validity of a mask descriptor.

procedure plvalid (pl)

pointer	pl		#I open mask descriptor
errchk	syserr

begin
	if (pl != NULL)
	    if (PL_MAGIC(pl) == PL_MAGICVAL)
		if (PL_LLBP(pl) == NULL)
		    call syserr (SYS_PLINACTDES)
		else
		    return

	call syserr (SYS_PLINVDES)
end
