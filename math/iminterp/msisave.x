# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSISAVE -- Procedure to save the interpolant for later use by MSIEVAL,
# MSIVECTOR, MSIDER and MSIGRL.

procedure msisave (msi, interpolant)

pointer	msi			# interpolant descriptor
real	interpolant[ARB]	# array containing the interpolant

int	npix

begin
	# save interpolant type, number of coefficients and position of
	# first data point
	MSI_SAVETYPE(interpolant) = MSI_TYPE(msi)
	MSI_SAVENSINC(interpolant) = MSI_NSINC(msi)
	MSI_SAVENXINCR(interpolant) = MSI_NXINCR(msi)
	MSI_SAVENYINCR(interpolant) = MSI_NYINCR(msi)
	MSI_SAVEXSHIFT(interpolant) = MSI_XSHIFT(msi)
	MSI_SAVEYSHIFT(interpolant) = MSI_YSHIFT(msi)
	MSI_SAVEXPIXFRAC(interpolant) = MSI_XPIXFRAC(msi)
	MSI_SAVEYPIXFRAC(interpolant) = MSI_YPIXFRAC(msi)
	MSI_SAVENXCOEFF(interpolant) = MSI_NXCOEFF(msi)
	MSI_SAVENYCOEFF(interpolant) = MSI_NYCOEFF(msi)
	MSI_SAVEFSTPNT(interpolant) = MSI_FSTPNT(msi)
	MSI_SAVEBADVAL(interpolant) = MSI_BADVAL(msi)

	# save coefficients
	call amovr (COEFF(MSI_COEFF(msi)), interpolant[MSI_SAVECOEFF+1],
	    MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi))

	# save look-up table
	if (MSI_NXINCR(msi) > 0 && MSI_NYINCR(msi) > 0) {
	    npix = (2 * MSI_NSINC(msi) + 1) ** 2 *
	        MSI_NXINCR(msi) * MSI_NYINCR(msi)
	    call amovr (LTABLE(MSI_LTABLE(msi)), interpolant[MSI_SAVECOEFF+1+
	        MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi)], npix)
	}
end
