# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIGETI -- Procedure to fetch an asi integer parameter

int procedure msigeti (msi, param)

pointer	msi		# interpolant descriptor
int	param		# parameter to be fetched

begin
	switch (param) {
	case II_MSITYPE:
	    return (MSI_TYPE(msi))
	case II_MSINSAVE:
	    return (MSI_NXCOEFF(msi) * MSI_NYCOEFF(msi) + MSI_SAVECOEFF)
	case II_MSINSINC:
	    return (MSI_NSINC(msi))
	default:
	    call error (0, "MSIGETI: Unknown MSI parameter.")
	}
end
