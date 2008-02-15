# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"
include	<math/iminterp.h>

# ASIRESTORE -- Procedure to restore the interpolant stored by ASISAVE
# for use by ASIEVAL, ASIVECTOR, ASIDER and ASIGRL.

procedure asirestore (asi, interpolant)

pointer	asi			# interpolant descriptor
real	interpolant[ARB]	# array containing the interpolant

int	interp_type, i, nconv
pointer	cptr

begin
	interp_type = int (ASI_SAVETYPE(interpolant))
	if (interp_type < 1 || interp_type > II_NTYPES)
	    call error (0, "ASIRESTORE: Unknown interpolant type.")

	# Allocate the interpolant descriptor structure and restore
	# interpolant parameters.

	call malloc (asi, LEN_ASISTRUCT, TY_STRUCT)
	ASI_TYPE(asi) = interp_type
	ASI_NSINC(asi) = nint (ASI_SAVENSINC(interpolant))
	ASI_NINCR(asi) = nint (ASI_SAVENINCR(interpolant))
	ASI_SHIFT(asi) = ASI_SAVESHIFT(interpolant)
	ASI_PIXFRAC(asi) = ASI_SAVEPIXFRAC(interpolant)
	ASI_NCOEFF(asi) = nint (ASI_SAVENCOEFF(interpolant))
	ASI_OFFSET(asi) = nint (ASI_SAVEOFFSET(interpolant))
	ASI_BADVAL(asi) = ASI_SAVEBADVAL(interpolant)

	# Allocate space for and restore coefficients.
	call malloc (ASI_COEFF(asi), ASI_NCOEFF(asi), TY_REAL)
	cptr = ASI_COEFF(asi) - 1
	do i = 1, ASI_NCOEFF(asi)
	    COEFF(cptr+i) = interpolant[ASI_SAVECOEFF+i] 

	# Allocate space for and restore the look-up tables.
	if (ASI_NINCR(asi) > 0) {
	    nconv = 2 * ASI_NSINC(asi) + 1
	    call malloc (ASI_LTABLE(asi), nconv * ASI_NINCR(asi), TY_REAL)
	    cptr = ASI_LTABLE(asi) - 1
	    do i = 1, nconv * ASI_NINCR(asi)
	        LTABLE(cptr+i) = interpolant[ASI_SAVECOEFF+ASI_NCOEFF(asi)+i] 
	} else
	    ASI_LTABLE(asi) = NULL
end
