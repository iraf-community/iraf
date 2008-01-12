# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGLPX -- Get a list structured complex parameter from the CL.

int procedure clglpx (param, xval)

char	param[ARB]
complex	xval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    return (EOF)
	else {
	    call gargx (xval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTNUM, param)
	}

	return (1)
end
