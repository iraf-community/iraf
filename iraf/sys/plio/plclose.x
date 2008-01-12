# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plio.h>

# PL_CLOSE -- Close a mask descriptor.  The memory resident mask is destroyed;
# an explicit call to one of the save procedures is required to save the mask
# in external storage.

procedure pl_close (pl)

pointer	pl			#I mask descriptor
errchk	syserr

begin
	if (pl != NULL) {
	    if (PL_MAGIC(pl) != PL_MAGICVAL)
		call syserr (SYS_PLINVDES)

	    if (PL_LPP(pl) != NULL)
		call mfree (PL_LPP(pl), TY_INT)
	    if (PL_LLBP(pl) != NULL)
		call mfree (PL_LLBP(pl), TY_SHORT)
	    call mfree (pl, TY_STRUCT)
	}
end
