# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

define	LUT_OMAX	1023

# IISOFM -- Read and Write OUTPUT look up table.
# Written data is from end points, read data is full
# array.

procedure iisofm (rw, color, offset, n, data)

short	rw			# read or write
short	color[ARB]		# color(s) to write
short	offset			# offset into lut
short	n			# number of data values
short	data[ARB]		# the data

int	command,len,x,y,z
int	mapcolor()
pointer	sp, odata

include "iis.com"

begin
	z = mapcolor (color)
	if ( rw == IDS_WRITE) {
	    if (n < 4)
	        return
	
	    call smark (sp)
	    call salloc (odata, LEN_OFM, TY_SHORT)
	    call aclrs (Mems[odata], LEN_OFM)

	    command = IWRITE+VRETRACE
	    call idslfill (data, int(n), Mems[odata], LEN_OFM, 0, LUT_OMAX)
	    len = LEN_OFM
	}
	else {
	    len = n
	    command = IREAD+VRETRACE
	}
	y = ADVYONXOV
	x = ADVXONTC
	call iishdr (command, len, OFM, x, y, z, 0)
	if (rw == IDS_WRITE) {
	    call iisio (Mems[odata], len * SZB_CHAR)
	    call sfree (sp)
	} else
	    call iisio (data, len * SZB_CHAR)
end
