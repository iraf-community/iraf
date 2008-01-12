# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <error.h>
include "mwcs.h"

# MW_CLOSE -- Close a MWCS descriptor and deallocate all resources used
# by the descriptor.  Any CTRAN descriptors which have been opened on
# the MWCS are automatically closed if not already manually closed by
# the application.

procedure mw_close (mw)

pointer	mw		#U pointer to MWCS descriptor

int	i
pointer	ct

begin
	# Free any still allocated CTRAN descriptors.
	do i = 1, MAX_CTRAN {
	    ct = MI_CTRAN(mw,i)
	    if (ct != NULL)
		iferr (call mw_ctfree (ct))
		    call erract (EA_WARN)
	}

	# Free the string and data buffers.
	if (MI_SBUF(mw) != NULL)
	    call mfree (MI_SBUF(mw), TY_CHAR)
	if (MI_DBUF(mw) != NULL)
	    call mfree (MI_DBUF(mw), TY_DOUBLE)

	# Free the main descriptor.
	call mfree (mw, TY_STRUCT)
end
