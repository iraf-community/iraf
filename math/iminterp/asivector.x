# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASIVECTOR -- Procedure to evaluate the interpolant at an array of ordered
# points assuming that all points land in 1 <= x <= npts.

procedure asivector (asi, x, y, npix)

pointer	asi		# interpolator descriptor
real	x[ARB]		# ordered x array
real	y[ARB]		# interpolated values
int	npix		# no. of points in x

begin
	switch (ASI_TYPE(asi))	{
	case II_NEAREST:
	    call ii_nearest (x, y, npix,
	        COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	case II_LINEAR:
	    call ii_linear (x, y, npix, COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	case II_POLY3:
	    call ii_poly3 (x, y, npix, COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	case II_POLY5:
	    call ii_poly5 (x, y, npix, COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	case II_SPLINE3:
	    call ii_spline3 (x, y, npix, COEFF(ASI_COEFF(asi)+ASI_OFFSET(asi)))
	case II_SINC:
	    call ii_sinc (x, y, npix,
		COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)), ASI_NCOEFF(asi),
		NSINC, NTAPER, STAPER, DX)
	default:
	    call error (0, "ASIVECTOR: Unknown interpolator type.")
	}
end
