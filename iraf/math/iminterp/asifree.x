# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im1interpdef.h"

# ASIFREE -- Procedure to deallocate sequential interpolant structure

procedure asifree (asi)

pointer	asi	# interpolant descriptor

begin
	if (ASI_COEFF(asi) != NULL)
	    call mfree (ASI_COEFF(asi), TY_REAL)
	if (ASI_LTABLE(asi) != NULL)
	    call mfree (ASI_LTABLE(asi), TY_REAL)
	call mfree (asi, TY_STRUCT)
end
