# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGETS -- Get a short integer valued parameter from the CL.

short procedure clgets (param)

char	param[ARB]
double	dval, clgetd()

begin
	dval = clgetd (param)
	if (IS_INDEFD (dval))
	    return (INDEFS)
	else
	    return (short(dval))
end
