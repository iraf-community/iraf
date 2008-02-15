# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASISINIT -- initialize the interpolant. This is a special entry point
# for the sinc interpolant although it will initialize the others too.

procedure asisinit (asi, interp_type, nsinc, nincr, shift, badval)

pointer	asi			# interpolant descriptor
int	interp_type		# interpolant type
int	nsinc			# sinc interpolant width
int	nincr			# number of sinc look-up table elements
real	shift			# sinc interpolant shift
real	badval			# drizzle bad pixel value

int	nconv

begin
	if (interp_type < 1 || interp_type > II_NTYPES)
	    call error (0, "ASISINIT: Illegal interpolant type")
	else {
	    call calloc (asi, LEN_ASISTRUCT, TY_STRUCT)
	    ASI_TYPE(asi) = interp_type
	    switch (interp_type) {
	    case II_LSINC:
		ASI_NSINC(asi) = (nsinc - 1) / 2
	        ASI_NINCR(asi) = nincr
		if (ASI_NINCR(asi) > 1)
		    ASI_NINCR(asi) = ASI_NINCR(asi) + 1
		if (nincr > 1)
		    ASI_SHIFT(asi) = INDEFR
		else
		    ASI_SHIFT(asi) = shift
		ASI_PIXFRAC(asi) = PIXFRAC
		nconv = 2 * ASI_NSINC(asi) + 1
		call calloc (ASI_LTABLE(asi), nconv * ASI_NINCR(asi),
		    TY_REAL)
		call ii_sinctable (Memr[ASI_LTABLE(asi)], nconv, ASI_NINCR(asi),
		    ASI_SHIFT(asi))
	    case II_SINC:
		ASI_NSINC(asi) = (nsinc - 1) / 2
	        ASI_NINCR(asi) = 0
		ASI_SHIFT(asi) = INDEFR
		ASI_PIXFRAC(asi) = PIXFRAC
		ASI_LTABLE(asi) = NULL
	    case II_DRIZZLE:
		ASI_NSINC(asi) = 0
	        ASI_NINCR(asi) = 0
		ASI_SHIFT(asi) = INDEFR
		ASI_PIXFRAC(asi) = max (MIN_PIXFRAC, min (shift, 1.0))
		ASI_LTABLE(asi) = NULL
	    default:
		ASI_NSINC(asi) = 0
	        ASI_NINCR(asi) = 0
		ASI_SHIFT(asi) = INDEFR
		ASI_PIXFRAC(asi) = PIXFRAC
		ASI_LTABLE(asi) = NULL
	    }
	    ASI_COEFF(asi) = NULL
	    ASI_BADVAL(asi) = badval
	}
end
