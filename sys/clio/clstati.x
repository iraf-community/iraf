# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<clio.h>
include	<clset.h>

# CLSTATI -- Get the value of an integer CLIO parameter.  Currently there is
# only one CLIO parameter, the process type of the parent (connected, detached,
# or host).

int procedure clstati (parameter)

int	parameter
include	"clio.com"

begin
	switch (parameter) {
	case CL_PRTYPE:
	    return (cl_prtype)
	case CL_PCACHE:
	    return (cl_stp)
	default:
	    call syserrs (SYS_CLSTATUS, "clstati")
	}
end
