# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<fio.h>
include	"m70.h"

# ZSTM70 -- Return device status for the IIS.

procedure zstm70 (chan, what, lvalue)

int	chan			# FCB pointer for device
int	what			# status parameter
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
