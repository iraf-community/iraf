# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

# IISTBALL -- Read, Write tball status to turn tball on/off

procedure iistball (rw, data)

short	rw			# read or write
short	data[ARB]		# the data

int	command,len
short	status
int	and(), or()

include "iis.com"

begin
	len = 1
	call iishdr (IREAD, len, CURSOR+COMMAND, 0, 0, 0, 0)
	call iisio (status, len * SZB_CHAR)
	if ( rw == IDS_WRITE) {
	    command = IWRITE+VRETRACE
	    switch (data[1]) {
	        case IDS_OFF:
		    status = and (int(status), 177771B)

	        case IDS_ON:
		    status = or ( int(status), 6)
	        }
	        call iishdr (command, 1, CURSOR+COMMAND, 0, 0, 0, 0)
	        call iisio (status, 1 * SZB_CHAR)
	} else {
	    if ( and ( int(status), 6) == 0 )
		data[2] = IDS_OFF
	    else
		data[2] = IDS_ON
	}
end
