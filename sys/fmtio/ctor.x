# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CTOR -- Character to real.  The number of characters converted to produce
# the output number is returned as the function value (0 is returned if the
# input cannot be interpreted as a number).

int procedure ctor (str, ip, rval)

char	str[ARB]		# input string to be decoded
int	ip			# first character to be used in string
real	rval			# decoded real value (output)

int	nchars
double	dval
int	ctod()

begin
	nchars = ctod (str, ip, dval)
	if (IS_INDEFD(dval))
	    rval = INDEFR
	else
	    rval = dval
	
	return (nchars)
end
