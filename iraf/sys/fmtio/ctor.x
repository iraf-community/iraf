# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>

# CTOR -- Character to real.  The number of characters converted to produce
# the output number is returned as the function value (0 is returned if the
# input cannot be interpreted as a number).

int procedure ctor (str, ip, rval)

char	str[ARB]		# input string to be decoded
int	ip			# first character to be used in string
real	rval			# decoded real value (output)

double	dval
int	nchars, expon
int	ctod()

begin
	nchars = ctod (str, ip, dval)
	if (abs(dval) > EPSILOND)
	    expon = int (log10 (abs(dval)))
	else
	    expon = 0

	if (IS_INDEFD(dval))
	    rval = INDEFR
	else if (expon > MAX_EXPONENTR)
	    return (0)
	else
	    rval = dval
	
	return (nchars)
end
