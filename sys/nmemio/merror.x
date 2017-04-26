# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>


# MERROR -- Provide a convenient trap for a memory error.

procedure merror (msg)

char	msg[ARB]

include "nmemio.com"

begin
	if (in_task > 0)
	    call error (EA_ERROR, msg)
end
