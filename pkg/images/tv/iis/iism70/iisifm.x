# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

define	LUT_IMAX	255

# IISIFM -- Read and Write INPUT look up table.
# Written data is from line end points, read data
# is full array.

procedure iisifm (rw, offset, n, data)

short	rw			# read or write
short	offset			# offset into lut
short	n			# number of data values
short	data[ARB]		# the data

int	command,len,x,y
pointer	sp, idata

include "iis.com"

begin
	if ( rw == IDS_WRITE) {
	    if (n < 4)
	        return

	    call smark (sp)
	    call salloc (idata, LEN_IFM, TY_SHORT)
	    call aclrs (Mems[idata], LEN_IFM)

	    command = IWRITE+VRETRACE
	    call idslfill (data, int(n), Mems[idata], LEN_IFM, 0, LUT_IMAX)
	    len = LEN_IFM
	} else {
	    len = n
	    command = IREAD+VRETRACE
	}

	y = ADVYONXOV
	x = ADVXONTC
	call iishdr (command, len, IFM, x, y, 0, 0)

	if (rw == IDS_WRITE) {
	    call iisio (Mems[idata], len * SZB_CHAR)
	    call sfree (sp)
	} else
	    call iisio (data, len * SZB_CHAR)
end
