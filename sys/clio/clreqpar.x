# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<clset.h>

# CLREQPAR -- Request a parameter from the CL.

procedure clreqpar (param)

char	param[ARB]
int	clstati()

begin
	call flush (STDOUT)

	if (clstati (CL_PRTYPE) == PR_CONNECTED) {
	    call putline (CLOUT, "=")
	    call putline (CLOUT, param)
	    call putline (CLOUT, "\n")
	} else {
	    call putline (CLOUT, param)
	    call putline (CLOUT, ": ")
	}

	call flush (CLOUT)
end
