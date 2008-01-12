# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ENVGETR -- Fetch an environment variable and try to interpret its value
# as a real.  Abort if variable is not found or cannot be converted to
# a number.

real procedure envgetr (varname)

char	varname[ARB]
double	val, envgetd()

begin
	val = envgetd (varname)
	if (IS_INDEFD(val))
	    return (INDEFR)
	else
	    return (val)
end
