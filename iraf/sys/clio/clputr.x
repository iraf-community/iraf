# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLPUTR -- Put a real valued parameter to the CL.

procedure clputr (param, rval)

char	param[ARB]
real	rval
double	dval

begin
	if (IS_INDEFR(rval))
	    dval = INDEFD
	else
	    dval = rval

	call clputd (param, dval)
end
