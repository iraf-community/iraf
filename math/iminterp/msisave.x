# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSISAVE -- Procedure to save the interpolant for later use by MSIEVAL,
# MSIVECTOR, MSIDER and MSIGRL.

procedure msisave (msi, interpolant)

pointer	msi			# interpolant descriptor
real	interpolant[ARB]	# array containing the interpolant

begin
	# save interpolant type, number of coefficients and position of
	# first data point
	MSI_SAVETYPE(interpolant) = MSI_TYPE(msi)
	MSI_SAVENXCOEFF(interpolant) = MSI_NXCOEFF(msi)
	MSI_SAVENYCOEFF(interpolant) = MSI_NYCOEFF(msi)
	MSI_SAVEFSTPNT(interpolant) = MSI_FSTPNT(msi)

	# save coefficients
	call amovr (COEFF(MSI_COEFF(msi)), interpolant[MSI_SAVECOEFF+1],
	    MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi))
end
