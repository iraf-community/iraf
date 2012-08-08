# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <fio.h>
include "zdisplay.h"
include "iis.h"

# IISSTT -- IIS status.
# [OBSOLETE - NO LONGER USED (see zsttim)]

procedure iisstt (chan, what, lvalue)

int	chan[ARB], what
long	lvalue

begin
	switch (what) {
	case FSTT_FILSIZE:
	    lvalue = IIS_FILSIZE
	case FSTT_BLKSIZE:
	    lvalue = IIS_BLKSIZE
	case FSTT_OPTBUFSIZE:
	    lvalue = IIS_OPTBUFSIZE
	case FSTT_MAXBUFSIZE:
	    lvalue = IIS_MAXBUFSIZE
	default:
	    lvalue = ERR
	}
end
