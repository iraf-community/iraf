# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<clset.h>
include	<syserr.h>
include	<clio.h>

# CLSETI -- Set a CLIO option of type integer.  Called by the IRAF Main
# upon process startup to set the CL_PRTYPE flag.

procedure clseti (parameter, value)

int	parameter		# CLIO parameter being queried
int	value			# value of parameter (output)
include	"clio.com"

begin
	switch (parameter) {
	case CL_PRTYPE:
	    cl_prtype = value
	default:
	    call syserrs (SYS_CLSETUKNPAR, "clseti")
	}
end
