# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>
include "im1interpdef.h"

# ASIINIT -- initialize the array sequential interpolant structure

procedure asiinit (asi, interp_type)

pointer	asi		# interpolant descriptor
int	interp_type	# interpolant type

int	nconv

begin
	if (interp_type < 1 || interp_type > II_NTYPES)
	    call error (0,"ASIINIT: Illegal interpolant type.")
	else {
	    call calloc (asi, LEN_ASISTRUCT, TY_STRUCT)
	    ASI_TYPE(asi) = interp_type
	    switch (interp_type) {
	    case II_LSINC:
	        ASI_NSINC(asi) = NSINC
		ASI_NINCR(asi) = NINCR
		if (ASI_NINCR(asi) > 1)
		    ASI_NINCR(asi) = ASI_NINCR(asi) + 1
		ASI_SHIFT(asi) = INDEFR
		ASI_PIXFRAC(asi) = PIXFRAC
		nconv = 2 * ASI_NSINC(asi) + 1
		call calloc (ASI_LTABLE(asi), nconv * ASI_NINCR(asi),
		    TY_REAL)
		call ii_sinctable (Memr[ASI_LTABLE(asi)], nconv, ASI_NINCR(asi),
		    ASI_SHIFT(asi))
	    case II_SINC:
	        ASI_NSINC(asi) = NSINC
		ASI_NINCR(asi) = 0
		ASI_SHIFT(asi) = INDEFR
		ASI_PIXFRAC(asi) = PIXFRAC
	        ASI_LTABLE(asi) = NULL
	    case II_DRIZZLE:
	        ASI_NSINC(asi) = 0
		ASI_NINCR(asi) = 0
		ASI_SHIFT(asi) = INDEFR
		ASI_PIXFRAC(asi) = PIXFRAC
	        ASI_LTABLE(asi) = NULL
	    default:
	        ASI_NSINC(asi) = 0
		ASI_NINCR(asi) = 0
		ASI_SHIFT(asi) = INDEFR
		ASI_PIXFRAC(asi) = PIXFRAC
	        ASI_LTABLE(asi) = NULL
	    }
	    ASI_BADVAL(asi) = BADVAL
	    ASI_COEFF(asi) = NULL
	}

end
