# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

define	LEN_CONST	3

# IISOFFSET -- Read and Write output bias registers

procedure iisoffset (rw, color, n, data)

short	rw			# read or write
short	color[ARB]		# color
short	n			# number of data values
short	data[ARB]		# the data

int	command,len,x
short	const[3]
int	i,j

include "iis.com"

begin
	command = IREAD+VRETRACE
	x = 8 + ADVXONTC
	len = LEN_CONST
	call iishdr(command, len, SUMPROC+COMMAND, x, 0, 0, 0)
	call iisio (const, len * SZB_CHAR)
	if ( rw == IDS_WRITE) {
	    command = IWRITE+VRETRACE
	    j = 1
	    for ( i =1 ; color[i] != IDS_EOD ; i = i + 1) {
		switch(color[i]) {
		    case IDS_RED:
			const[3] = data[j]

		    case IDS_GREEN:
			const[2] = data[j]

		    case IDS_BLUE:
			const[1] = data[j]
		}
		if ( j < n)
		    j = j + 1
	    }
	    call iishdr (command, len, SUMPROC+COMMAND, x, 0, 0, 0)
	    call iisio (const, len * SZB_CHAR)
	} else {
	    j = 1
	    for ( i = 1 ; i <= n ; i = i + 1 ) {
		switch(color[j]) {
		    case IDS_RED:
			data[i] = const[3]

		    case IDS_GREEN:
			data[i] = const[2]

		    case IDS_BLUE:
			data[i] = const[1]
		}
		j = j+1
		if ( color[j] == IDS_EOD )
		    j = j - 1
	    }
	}
end
