# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include	<math/iminterp.h>

# MSIRESTORE -- Procedure to restore the interpolant stored by MSISAVE
# for use by MSIEVAL, MSIVECTOR, MSIDER and MSIGRL.

procedure msirestore (msi, interpolant)

pointer	msi			# interpolant descriptor
real	interpolant[ARB]	# array containing the interpolant

int	interp_type

begin
	interp_type = int (MSI_SAVETYPE(interpolant))
	if (interp_type < 1 || interp_type > II_NTYPES)
	    call error (0, "MSIRESTORE: Unknown interpolant type.")

	# allocate the interpolant descriptor structure and restore
	# interpolant parameters
	call malloc (msi, LEN_MSISTRUCT, TY_STRUCT)
	MSI_TYPE(msi) = interp_type
	MSI_NXCOEFF(msi) = int (MSI_SAVENXCOEFF(interpolant))
	MSI_NYCOEFF(msi) = int (MSI_SAVENYCOEFF(interpolant))
	MSI_FSTPNT(msi) = int (MSI_SAVEFSTPNT(interpolant))

	# allocate space for and restore coefficients
	call malloc (MSI_COEFF(msi), MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi),
	    TY_REAL)
	call amovr (interpolant[1+MSI_SAVECOEFF], COEFF(MSI_COEFF(msi)),
	    MSI_NXCOEFF(msi), MSI_NYCOEFF(msi))
end
