# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

define	LEN_MM	6

# IISMIN -- Read minimum registers

procedure iismin (rw, color, n, data)

short	rw			# read or write
short	color[ARB]		# color
short	n			# number of data values
short	data[ARB]		# the data

int	command,x
short	const[LEN_MM]
int	i,j

include "iis.com"

begin
	if ( rw == IDS_WRITE)
	    return
	command = IREAD+VRETRACE
	x = ADVXONTC
	call iishdr(command, LEN_MM, SUMPROC+COMMAND, x, 0, 0, 0)
	call iisio (const, LEN_MM * SZB_CHAR)
	j = 1
	for ( i = 1 ; i <= n ; i = i + 1 ) {
	    switch(color[j]) {
		case IDS_RED:
		    data[i] = const[5]

		case IDS_GREEN:
		    data[i] = const[3]

		case IDS_BLUE:
		    data[i] = const[1]
	    }
	    j = j+1
	    if ( color[j] == IDS_EOD )
	        j = j - 1
	}
end

# IISMAX -- Read maximum registers

procedure iismax (rw, color, n, data)

short	rw			# read or write
short	color[ARB]		# color
short	n			# number of data values
short	data[ARB]		# the data

int	command,x
short	const[LEN_MM]
int	i,j

include "iis.com"

begin
	if ( rw == IDS_WRITE)
	    return
	command = IREAD+VRETRACE
	x = ADVXONTC
	call iishdr(command, LEN_MM, SUMPROC+COMMAND, x, 0, 0, 0)
	call iisio (const, LEN_MM * SZB_CHAR)
	j = 1
	for ( i = 1 ; i <= n ; i = i + 1 ) {
	    switch(color[j]) {
		case IDS_RED:
		    data[i] = const[6]

		case IDS_GREEN:
		    data[i] = const[4]

		case IDS_BLUE:
		    data[i] = const[2]
	    }
	    j = j+1
	    if ( color[j] == IDS_EOD )
		j = j - 1
	}
end
