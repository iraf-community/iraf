# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGETX -- Get a complex parameter from the CL.

complex procedure clgetx (param)

char	param[ARB]
complex	xval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    call syserrs (SYS_CLEOFNLP, param)
	else {
	    call gargx (xval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTNUM, param)
	}

	return (xval)
end
