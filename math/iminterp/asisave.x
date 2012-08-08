# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"
include <math/iminterp.h>

# ASISAVE -- Procedure to save the interpolant for later use by ASIEVAL,
# ASIVECTOR, ASIDER and ASIGRL.

procedure asisave (asi, interpolant)

pointer	asi			# interpolant descriptor
real	interpolant[ARB]	# array containing the interpolant

int	i, nconv
pointer	cptr

begin
	# Save the interpolant type, number of coefficients, and position of
	# first data point.

	ASI_SAVETYPE(interpolant) = ASI_TYPE(asi)
	ASI_SAVENSINC(interpolant) = ASI_NSINC(asi)
	ASI_SAVENINCR(interpolant) = ASI_NINCR(asi)
	ASI_SAVESHIFT(interpolant) = ASI_SHIFT(asi)
	ASI_SAVEPIXFRAC(interpolant) = ASI_PIXFRAC(asi)
	ASI_SAVENCOEFF(interpolant) = ASI_NCOEFF(asi)
	ASI_SAVEOFFSET(interpolant) = ASI_OFFSET(asi)
	ASI_SAVEBADVAL(interpolant) = ASI_BADVAL(asi)

	# Save the coefficients.
	cptr = ASI_COEFF(asi) - 1
	do i = 1, ASI_NCOEFF(asi)
	    interpolant[ASI_SAVECOEFF+i] = COEFF(cptr+i)

	# Save the lookup-tables.
	if (ASI_NINCR(asi) > 0) {
	    nconv = 2 * ASI_NSINC(asi) + 1
	    cptr = ASI_LTABLE(asi) - 1
	    do i = 1, nconv * ASI_NINCR(asi)
		interpolant[ASI_SAVECOEFF+ASI_NCOEFF(asi)+i] = LTABLE(cptr+i)
	}
end
