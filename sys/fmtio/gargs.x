# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# GARGS -- Interpret the next input token as an integer quantity.

procedure gargs (sval)

short	sval
double	dval

begin
	call gargd (dval)
	if (IS_INDEFD (dval))
	    sval = INDEFS
	else if (abs(dval) > MAX_SHORT)
	    sval = INDEFS
	else
	    sval = dval
end
