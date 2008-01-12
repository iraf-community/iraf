# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "iis.h"
include "../lib/ids.h"

# IISBUTTON -- Read, button status

procedure iisbutton (cnum, x, y, key)

int	cnum			# cursor number
int	x,y			# coordinates
int	key			# key pressed

short	status
int	and()

include "iis.com"

begin
	call iishdr (IREAD, 1, CURSOR+COMMAND, 0, 0, 0, 0)
	call iisio (status, 1 * SZB_CHAR)

	if ( cnum == IDS_BUT_WT ) {
	    while ( and (int(status), PUSH) == 0 ) {
		call tsleep(1)
		call iisio (status, 1 * SZB_CHAR)
	    }
	}

	if ( and ( int(status), PUSH) == 0 )
	    key = 0
	else {
	    status = and ( int(status), 7400B) / 256
	    switch(status) {
		case 4:
		    status = 3
		
		case 8:
		    status = 4
	    }
	    key = status
	}
end
