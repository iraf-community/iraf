# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGLSTR -- Get a list structured string parameter from the CL.

int procedure clglstr (param, outstr, maxch)

char	param[ARB], outstr[maxch]
int	maxch
int	clscan(), nscan(), strlen()

begin
	if (clscan (param) == EOF)
	    return (EOF)
	else {
	    call gargstr (outstr, maxch)
	    if (nscan() != 1)
		outstr[1] = EOS
	}

	return (strlen (outstr))
end
