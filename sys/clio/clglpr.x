# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGLPR -- Get a list structured single precision floating valued parameter
# from the CL.

int procedure clglpr (param, rval)

char	param[ARB]
real	rval
int	stat, clglpd()
double	dval

begin
	stat = clglpd (param, dval)
	if (IS_INDEFD (dval))
	    rval = INDEFR
	else
	    rval = real (dval)
	return (stat)
end
