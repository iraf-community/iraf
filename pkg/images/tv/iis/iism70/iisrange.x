# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

define	LEN_RANGE	1

# IISRANGE -- Read and write range scaling registers
# Input data is of form 1-->range "0", 2,3 --> "1", 4-7 --> "2"
# and anything beyond 7 --> "4".  This is just like zoom.
# However, on readback, the actual range values are returned.  If
# this should change, the zsnapinit code must change too (the only
# place where a range read is done).

procedure iisrange (rw, color, n, data)

short	rw			# read or write
short	color[ARB]		# color
short	n			# number of data values
short	data[ARB]		# the data

short	range
int	i, j
int	command, x, itemp, ival
int	and(), or()
include "iis.com"

begin
	if (data[1] == IDS_EOD)
	    return

	command = IREAD
	x = ADVXONTC

	call iishdr (command, LEN_RANGE, OFM+COMMAND, x, 0, 0, 0)
	call iisio (range, LEN_RANGE * SZB_CHAR)

	if (rw == IDS_WRITE) {
	    command = IWRITE+VRETRACE
	    j = 1
	    for (i=1;  color[i] != IDS_EOD;  i=i+1) {
		switch (data[j]) {
		case 1,2:
		    ival = data[j]-1
		case 3:
		    ival = 1
		case 4,5,6,7:
		    ival = 2

		default:
		    if (ival < 0)
			ival = 0
		    else
			ival = 3
		}

		itemp = range
		switch(color[i]) {
		case IDS_RED:
		    range = or (ival*16, and (itemp, 17B))

		case IDS_GREEN:
		    range = or (ival*4, and (itemp, 63B))

		case IDS_BLUE:
		    range = or (ival, and (itemp, 74B))
		}

		if ( j < n)
		    j = j + 1
	    }

	    call iishdr (command, LEN_RANGE, OFM+COMMAND, x, 0, 0, 0)
	    call iisio (range, LEN_RANGE * SZB_CHAR)

	} else {
	    # Return a  range value
	    j = 1
	    for (i=1;  i <= n;  i=i+1) {
		itemp = range
		switch (color[j]) {
		case IDS_RED:
		    data[i] = and (itemp, 60B) / 16

		case IDS_GREEN:
		    data[i] = and (itemp, 14B) / 4

		case IDS_BLUE:
		    data[i] = and (itemp, 3B)
		}
		j = j+1
		if (color[j] == IDS_EOD)
		    j = j - 1
	    }
	}
end
