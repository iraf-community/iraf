# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGLPS -- Get a list structured short integer valued parameter from the CL.

int procedure clglps (param, sval)

char	param[ARB]
short	sval
int	stat, clglpd()
double	dval

begin
	stat = clglpd (param, dval)
	if (IS_INDEFD (dval))
	    sval = INDEFS
	else
	    sval = short (dval)
	return (stat)
end
