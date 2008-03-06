# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"fmio.h"

# FM_LFAWAIT -- Wait for i/o on an lfile.

procedure fm_lfawait (lf_chan, status)

int	lf_chan			#I lfile descriptor
long	status			#O i/o status (nbytes transferred or ERR)

pointer	lf

include "fmio.com"

begin
	lf = Memp[lf_ptrs+lf_chan]
	call fm_lfbinwait (lf_chan, status)
	if (and (LF_FLAGS(lf), LFF_TEXTFILE) != 0)
	    if (status > 0)
		status = status * SZB_CHAR
end
