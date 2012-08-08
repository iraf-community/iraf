# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"fmio.h"

# FM_LFAWAIT -- Wait for i/o on an lfile.

procedure fm_lfawait (lf, status)

pointer	lf			#I lfile descriptor
int	status			#O i/o status (nbytes transferred or ERR)

begin
	call fm_lfbinwait (lf, status)
	if (and (LF_FLAGS(lf), LFF_TEXTFILE) != 0)
	    if (status > 0)
		status = status * SZB_CHAR
end
