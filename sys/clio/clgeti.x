# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGETI -- Get an integer valued parameter from the CL.

int procedure clgeti (param)

char	param[ARB]
double	dval, clgetd()

begin
	dval = clgetd (param)
	if (IS_INDEFD (dval))
	    return (INDEFI)
	else
	    return (int(dval))
end
