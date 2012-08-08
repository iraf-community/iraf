# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

# cscale makes 0-32767 range from 0-62.  The 62 results from the need
# to describe a cursor with a center, and hence an ODD number of points.
# Thus, we pretend the cursor ranges from 0-62 rather than 0-63, and
# the center is at (31,31).
# cwidth describes the (cursor) ram width, which is 64 ( by 64).

define	CSCALE	528
define	CWIDTH	 64
define	CSIZE	4096

# IISCURSOR -- Read, Write cursor shape, turn cursor on/off

procedure iiscursor (rw, cur, n, data)

short	rw			# read or write
short	cur			# cursor number ... ignored for IIS M70
short	n			# number of data values
short	data[ARB]		# the data

short	command, len
short	shape[CSIZE]
short	status
int	rate
int	i,j,index
int	mod(), and(), or(), andi()

include "iis.com"

begin
	len = 1
	if (data[1] != IDS_CSHAPE) {
	    call iishdr (IREAD, len, CURSOR+COMMAND, 0, 0, 0, 0)
	    call iisio (status, len * SZB_CHAR)
	}

	if (rw == IDS_WRITE)
	    command = andi (IWRITE+VRETRACE, 177777B)
	else
	    command = andi (IREAD+VRETRACE, 177777B)

	if (data[1] != IDS_CSHAPE){
	    if (rw == IDS_WRITE) {
	        switch (data[1]) {
		    case IDS_OFF:
		        status = and(int(status), 177776B)

		    case IDS_ON:
		        status = or (int(status), 1)

		    case IDS_CBLINK:
		        rate = mod (int(data[2])-1, 4) * 8
		        status = or (rate, and (int(status),177747B))
	        }
	        call iishdr (command, len, CURSOR+COMMAND, 0, 0, 0, 0)
	        call iisio (status, len * SZB_CHAR)
	    } else {
		if ( data[1] == IDS_CBLINK )
		    data[2] = ( and (int(status), 30B) / 8 ) + 1
		else if ( and ( int(status), 1) == 0 )
		    data[1] = IDS_OFF
		else
		    data[1] = IDS_ON
	    }

	} else {
	    # deal with cursor shape.

	    len = CSIZE
	    if ( rw == IDS_WRITE) {
	        call aclrs (shape, CSIZE)
	        for ( i = 2 ; i <= n-1 ; i = i + 2 ) {
	    # given GKI data pairs for x,y cursor_on bits, set shape datum
	    # the first value is x, then y
		    if (data[i] == IDS_EOD)
			break
		    j = data[i]/CSCALE
		    index = (data[i+1]/CSCALE) * CWIDTH + j + 1
		    shape[index] = 1
	        }
	    }

	    call iishdr (command, len, CURSOR, ADVXONTC, ADVYONXOV, 0, 0)
	    call iisio (shape, len * SZB_CHAR)

	    # if read command, return all set bits as GKI x,y pairs
	    if ( rw != IDS_WRITE) {
		i = 2
		for ( j = 1 ;  j <= CSIZE ; j = j + 1 ) {
		    if ( shape[j] != 0 ) {
			data[i] = mod(j,CWIDTH) * CSCALE
			data[i+1] = (j/CWIDTH) * CSCALE
			i = i + 2
			if ( i > n-1 )
			    break
		    }
		}
		if ( i <= n ) 
		    data[i] = IDS_EOD
		n = i
	    }
	}
end
