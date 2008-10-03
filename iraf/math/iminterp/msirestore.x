# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include	<math/iminterp.h>

# MSIRESTORE -- Procedure to restore the interpolant stored by MSISAVE
# for use by MSIEVAL, MSIVECTOR, MSIDER and MSIGRL.

procedure msirestore (msi, interpolant)

pointer	msi			# interpolant descriptor
real	interpolant[ARB]	# array containing the interpolant

size_t	sz_val
int	interp_type
size_t	npix
int	nint_ri()

begin
	interp_type = nint_ri (MSI_SAVETYPE(interpolant))
	if (interp_type < 1 || interp_type > II_NTYPES)
	    call error (0, "MSIRESTORE: Unknown interpolant type.")

	# allocate the interpolant descriptor structure and restore
	# interpolant parameters
	sz_val = LEN_MSISTRUCT
	call malloc (msi, sz_val, TY_STRUCT)
	MSI_TYPE(msi) = interp_type
	MSI_NSINC(msi) = nint_ri (MSI_SAVENSINC(interpolant))
	MSI_NXINCR(msi) = nint_ri (MSI_SAVENXINCR(interpolant))
	MSI_NYINCR(msi) = nint_ri (MSI_SAVENYINCR(interpolant))
	MSI_XSHIFT(msi) = MSI_SAVEXSHIFT(interpolant)
	MSI_YSHIFT(msi) = MSI_SAVEYSHIFT(interpolant)
	MSI_XPIXFRAC(msi) = MSI_SAVEXPIXFRAC(interpolant)
	MSI_YPIXFRAC(msi) = MSI_SAVEYPIXFRAC(interpolant)
	MSI_NXCOEFF(msi) = nint_ri (MSI_SAVENXCOEFF(interpolant))
	MSI_NYCOEFF(msi) = nint_ri (MSI_SAVENYCOEFF(interpolant))
	MSI_FSTPNT(msi) = nint_ri (MSI_SAVEFSTPNT(interpolant))
	MSI_BADVAL(msi) = MSI_SAVEBADVAL(interpolant)

	# allocate space for and restore coefficients
	sz_val = MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi)
	call malloc (MSI_COEFF(msi), sz_val, TY_REAL)
	call amovr (interpolant[1+MSI_SAVECOEFF], COEFF(MSI_COEFF(msi)), sz_val)

	# allocate space for and restore the look-up table
	if (MSI_NXINCR(msi) > 0 && MSI_NYINCR(msi) > 0) {
	    npix = (2.0 * MSI_NSINC(msi) + 1) ** 2 * MSI_NXINCR(msi) *
		MSI_NYINCR(msi)
	    call amovr (interpolant[1+MSI_SAVECOEFF+MSI_NXCOEFF(msi) *
	        MSI_NYCOEFF(msi)], LTABLE(MSI_LTABLE(msi)), npix)
	}
end
