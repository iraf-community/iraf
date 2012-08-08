# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGETL -- Get a long integer parameter from the CL.

long procedure clgetl (param)

char	param[ARB]
double	dval, clgetd()

begin
	dval = clgetd (param)
	if (IS_INDEFD (dval))
	    return (INDEFL)
	else
	    return (long(dval))
end
