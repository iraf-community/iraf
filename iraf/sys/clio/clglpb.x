# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGLPB -- Get a list structured boolean parameter from the CL.

int procedure clglpb (param, bval)

char	param[ARB]
bool	bval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    return (EOF)
	else {
	    call gargb (bval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTBOOL, param)
	}

	return (1)
end
