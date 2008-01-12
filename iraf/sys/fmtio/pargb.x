# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# PARGB -- Print a boolean operand (as a string).

procedure pargb (bval)

bool	bval

begin
	if (bval)
	    call pargstr ("yes")
	else
	    call pargstr ("no")
end
