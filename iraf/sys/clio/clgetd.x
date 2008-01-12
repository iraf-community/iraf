# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGETD -- Get a double precision floating parameter from the CL.

double procedure clgetd (param)

char	param[ARB]
double	dval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    call syserrs (SYS_CLEOFNLP, param)
	else {
	    call gargd (dval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTNUM, param)
	}

	return (dval)
end
