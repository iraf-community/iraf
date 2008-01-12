# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# procedure to evaluate interpolator at a sequence of ordered points
# aasumes that all x_values land within [1,n].

procedure asieva(x, y, n, coeff)
include "interpdef.h"
include "asidef.h"

real x[ARB]			# ordered x array
int n				# no. of points in x
real coeff[ARB]

real y[ARB]			# interpolated values

begin
	switch (ITYPEI)	{	# switch on interpolator type
	
	case IT_NEAREST :
	    call iievne(x, y, n, coeff[COFF + 1])

	case IT_LINEAR :
	    call iievli(x, y, n, coeff[COFF + 1])

	case IT_POLY3 :
	    call iievp3(x, y, n, coeff[COFF + 1])

	case IT_POLY5 :
	    call iievp5(x, y, n, coeff[COFF + 1])

	case IT_SPLINE3 :
	    call iievs3(x, y, n, coeff[COFF + 1])

	}

	return

end
