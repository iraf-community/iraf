# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# CLGSTR -- Get a string parameter from the CL.

procedure clgstr (param, outstr, maxch)

char	param[ARB], outstr[maxch]
int	maxch
int	clscan(), nscan()

begin
	if (clscan (param) == EOF)
	    call syserr (SYS_CLEOFNLP)
	else {
	    call gargstr (outstr, maxch)
	    if (nscan() != 1)
		outstr[1] = EOS
	}
end
