# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGETB -- Get a boolean parameter from the CL.

bool procedure clgetb (param)

char	param[ARB]
bool	bval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    call syserrs (SYS_CLEOFNLP, param)
	else {
	    call gargb (bval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTBOOL, param)
	}

	return (bval)
end
