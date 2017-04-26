# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include <math/iminterp.h>

# MSIGETR -- Procedure to fetch an msi real parameter

real procedure msigetr (msi, param)

pointer	msi		# interpolant descriptor
int	param		# parameter to be fetched

begin
	switch (param) {
	case II_MSIBADVAL:
	    return (MSI_BADVAL(msi))
	default:
	    call error (0, "MSIGETR: Unknown MSI parameter.")
	}
end
