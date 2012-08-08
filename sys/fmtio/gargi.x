# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# GARGI -- Interpret the next input token as an integer quantity.

procedure gargi (ival)

int	ival
double	dval

begin
	call gargd (dval)
	if (IS_INDEFD (dval))
	    ival = INDEFI
	else if (abs(dval) > MAX_INT)
	    ival = INDEFI
	else
	    ival = dval
end
