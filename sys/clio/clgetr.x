# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGETR -- Get a single precision floating parameter from the CL.

real procedure clgetr (param)

char	param[ARB]
double	dval, clgetd()

begin
	dval = clgetd (param)
	if (IS_INDEFD (dval))
	    return (INDEFR)
	else
	    return (real(dval))
end
