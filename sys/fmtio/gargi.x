# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGI -- Interpret the next input token as an integer quantity.

procedure gargi (ival)

int	ival
double	dval

begin
	call gargd (dval)
	ival = dval
	if (IS_INDEFD (dval))
	    ival = INDEFI
end
