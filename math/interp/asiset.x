# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# sets sequential interpolator type

procedure asiset(coeff,interptype)
include "interpdef.h"
include "asidef.h"

int interptype
real coeff[ARB]

begin

	if(interptype <= 0 || interptype > ITNIT)
	    call error(ASITYPEERR,"illegal interpolator type")
	else
	    TYPEI = interptype

	return
end
