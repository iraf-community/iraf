# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# GARGL -- Interpret the next input token as an integer quantity.

procedure gargl (lval)

long	lval
double	dval

begin
	call gargd (dval)
	if (IS_INDEFD (dval))
	    lval = INDEFL
	else if (abs(dval) > MAX_LONG)
	    lval = INDEFL
	else
	    lval = dval
end
