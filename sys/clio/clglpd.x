# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGLPD -- Get a list structured double precision floating parameter from
# the CL.

int procedure clglpd (param, dval)

char	param[ARB]
double	dval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    return (EOF)
	else {
	    call gargd (dval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTNUM, param)
	}

	return (1)
end
