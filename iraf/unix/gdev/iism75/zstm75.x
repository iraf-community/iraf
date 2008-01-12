# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<fio.h>
include	"m75.h"

# ZSTM75 -- Return device status for the IIS.

procedure zstm75 (ifcb, what, lvalue)

int	ifcb			# pointer to channel descriptor passed as int
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
