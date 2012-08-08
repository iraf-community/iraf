# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLGLPL -- Get a list structured long integer valued parameter from the CL.

int procedure clglpl (param, lval)

char	param[ARB]
long	lval
int	stat, clglpd()
double	dval

begin
	stat = clglpd (param, dval)
	if (IS_INDEFD (dval))
	    lval = INDEFL
	else
	    lval = long (dval)
	return (stat)
end
