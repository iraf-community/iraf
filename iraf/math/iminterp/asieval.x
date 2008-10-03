# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASIEVAL -- This procedure finds the interpolated value assuming that
# x lands in the array, i.e. 1 <= x <= npts.

real procedure asieval (asi, x)

pointer	asi		# interpolator descriptor
real x[ARB]		# x value

size_t	sz_val
size_t	c_1
real value

begin
	c_1 = 1

	switch (ASI_TYPE(asi))	{	# switch on interpolator type
	
	case II_NEAREST:
	    call ii_nearest (x, value, c_1,
	    		    COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	    return (value)

	case II_LINEAR:
	    call ii_linear (x, value, c_1,
	    		   COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	    return (value)

	case II_POLY3:
	    call ii_poly3 (x, value, c_1, COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	    return (value)

	case II_POLY5:
	    call ii_poly5 (x, value, c_1, COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	    return (value)

	case II_SPLINE3:
	    call ii_spline3 (x, value, c_1,
	    		    COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)))
	    return (value)

	case II_SINC:
	    sz_val = ASI_NCOEFF(asi)
	    call ii_sinc (x, value, c_1, COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)),
			  sz_val, ASI_NSINC(asi), DX)
	    return (value)
	
	case II_LSINC:
	    sz_val = ASI_NCOEFF(asi)
	    call ii_lsinc (x, value, c_1,
			   COEFF(ASI_COEFF(asi) + ASI_OFFSET(asi)), sz_val,
			   LTABLE(ASI_LTABLE(asi)), 2 * ASI_NSINC(asi) + 1,
			   ASI_NINCR(asi), DX)
	    return (value)

	case II_DRIZZLE:
	    if (ASI_PIXFRAC(asi) >= 1.0)
	        call ii_driz1 (x, value, c_1, COEFF(ASI_COEFF(asi) +
		    ASI_OFFSET(asi)), ASI_BADVAL(asi))
	    else
	        call ii_driz (x, value, c_1, COEFF(ASI_COEFF(asi) +
		    ASI_OFFSET(asi)), ASI_PIXFRAC(asi), ASI_BADVAL(asi))
	    return (value)

	default:
	    call error (0, "ASIEVAL: Unknown interpolator type.")
	}
end
