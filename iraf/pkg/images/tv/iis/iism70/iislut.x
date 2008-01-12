# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

define	LUT_LMAX	255

# IISLUT -- Read and Write look up table.
# NOTE the ASYMMETRY ... written data is derived from end
# points, but read data is the full array (see zsnapinit,
# for instance, for read usage.)

procedure iislut (rw, frame, color, offset, n, data)

short	rw			# read or write
short	frame[ARB]		# frame array
short	color[ARB]		# color array
short	offset			# offset into lut
short	n			# number of data values
short	data[ARB]		# the data

int	command,len,x,y,z,t
short	iispack()
int	mapcolor()
pointer	sp, ldata

include "iis.com"

begin
	z = mapcolor (color)
	t = iispack(frame)
	if (t == GRCHAN) {
	    return
	}

	if ( rw == IDS_WRITE) {
	    if ( n < 4)
	        return
	    command = IWRITE+VRETRACE

	    # data space for manipulating lut information

	    call smark (sp)
	    call salloc (ldata, LEN_LUT, TY_SHORT)
	    call aclrs (Mems[ldata], LEN_LUT)

	    # We could have negative lut values, but don't bother for now
	    call idslfill (data, int(n), Mems[ldata], LEN_LUT, 0, LUT_LMAX)

	    len = LEN_LUT
	} else {
	    len = n
	    command = IREAD+VRETRACE
	}

	x = ADVXONTC
	y = 0

	call iishdr (command, len, LUT, x, y, z, t)

	if ( rw == IDS_WRITE) {
	    call iisio (Mems[ldata], len * SZB_CHAR)
	    call sfree (sp)
	} else
	    call iisio (data, len * SZB_CHAR)
end
