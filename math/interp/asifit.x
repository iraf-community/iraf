# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		asifit -- fit interpolator to data

This version uses the "basis-spline" representation for the spline.
It stores the data array itself for the polynomial interpolants.

.endhelp

procedure asifit(datain,n,coeff)
include "interpdef.h"
include "asidef.h"

real datain[ARB]	# data array
int n			# no. of data points
real coeff[ARB]

int i

begin
	NPTS = n

	# error trap - check data array for size
	switch (ITYPEI) {
	case IT_SPLINE3:
	    if(n < 4)
		call error(ASIFITERR,"too few points for spline")
	case IT_POLY5:
	    if(n < 6)
		call error(ASIFITERR,"too few points for poly5")
	case IT_POLY3:
	    if(n < 4)
		call error(ASIFITERR,"too few points for poly3")
	case IT_LINEAR:
	    if(n < 2)
		call error(ASIFITERR,"too few points for linear")
	default:
	    if(n < 1)
		call error(ASIFITERR,"too few points for interpolation")

	}

	do i = 1,n
	    coeff[COFF + i] = datain[i]


	if (ITYPEI == IT_SPLINE3) {

	    # specify natural end conditions - second deriv. zero
	    coeff[COFF] = 0.
	    coeff[COFF+n+1] = 0.

	    # fit spline - generate b-spline coefficients.
	    call iif_spline(coeff[COFF],coeff[COFF + n + 2],n)


	} else {   # not the spline
	    # We extend array to take care of values near edge.

	    # Assign arbitrary values in case there are only a few points.
	    for(i = n + 1; i < 5; i = i + 1)
		coeff[COFF + i] = coeff[COFF + n]

	    # Extend for worst case - poly 5 may need 3 extra points.
	    do i = 1,3 {   # same recipe as project
		coeff[COFF+1 - i] = 2. * coeff[COFF + 1] -
			      coeff[COFF + 1 + i]
		coeff[COFF+n + i] = 2. * coeff[COFF + n] -
			      coeff[COFF + n - i]
	    }
	}

	return
end
