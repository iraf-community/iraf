# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGL -- Interpret the next input token as an integer quantity.

procedure gargl (lval)

long	lval
double	dval

begin
	call gargd (dval)
	lval = dval
	if (IS_INDEFD (dval))
	    lval = INDEFL
end
