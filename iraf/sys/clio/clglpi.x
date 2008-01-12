# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGLPI -- Get a list structured integer valued parameter from the CL.

int procedure clglpi (param, ival)

char	param[ARB]
int	ival, stat, clglpd()
double	dval

begin
	stat = clglpd (param, dval)
	if (IS_INDEFD (dval))
	    ival = INDEFI
	else
	    ival = int (dval)
	return (stat)
end
