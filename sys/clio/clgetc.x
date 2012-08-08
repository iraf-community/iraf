# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGETC -- Get a character constant from the CL.

char procedure clgetc (param)

char	param[ARB]
char	cval
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    call syserrs (SYS_CLEOFNLP, param)
	else {
	    call gargc (cval)
	    if (nscan() != 1)
		call syserrs (SYS_CLNOTCC, param)
	}

	return (cval)
end
