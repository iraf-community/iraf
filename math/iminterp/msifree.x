# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"

# MSIFREE -- Procedure to deallocate the interpolant descriptor structure.

procedure msifree (msi)

pointer	msi		# pointer to the interpolant descriptor structure
errchk	mfree

begin
	# free coefficient array
	if (MSI_COEFF(msi) != NULL)
	    call mfree (MSI_COEFF(msi), TY_REAL)
	if (MSI_LTABLE(msi) != NULL)
	    call mfree (MSI_LTABLE(msi), TY_REAL)

	# free interpolant descriptor
	call mfree (msi, TY_STRUCT)
end
