# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGLPC -- Get a list structured character constant parameter from the CL.

int procedure clglpc (param, cval)

char	param[ARB]
char	cval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    return (EOF)
	else {
	    call gargc (cval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTCC, param)
	}

	return (1)
end
