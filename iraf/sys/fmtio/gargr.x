# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGR -- Interpret the next input token as a single precision floating
# quantity.

procedure gargr (rval)

real	rval
double	dval

begin
	call gargd (dval)
	if (IS_INDEFD (dval))
	    rval = INDEFR
	else
	    rval = dval
end
