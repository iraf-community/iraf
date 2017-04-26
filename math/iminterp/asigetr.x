# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"
include <math/iminterp.h>

# ASIGETR -- Procedure to fetch an msi real parameter

real procedure asigetr (asi, param)

pointer	asi		# interpolant descriptor
int	param		# parameter to be fetched

begin
	switch (param) {
	case II_ASIBADVAL:
	    return (ASI_BADVAL(asi))
	default:
	    call error (0, "ASIGETR: Unknown ASI parameter.")
	}
end
