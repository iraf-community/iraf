# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGS -- Interpret the next input token as an integer quantity.

procedure gargs (sval)

short	sval
double	dval

begin
	call gargd (dval)
	sval = dval
	if (IS_INDEFD (dval))
	    sval = INDEFS
end
