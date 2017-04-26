include <math/nlfit.h>
include "nlfitdefr.h"


# NLEVAL -- Evaluate the fit at a point.

real procedure nlevalr (nl, x, nvars)

pointer	nl		# nlfit descriptor
real	x[ARB]		# x values
int	nvars		# number of variables

real	zfit

begin
	# Evaluate the function.
	call zcall5 (NL_FUNC(nl), x, nvars, PARAM(NL_PARAM(nl)),
	    NL_NPARAMS(nl), zfit)

	return (zfit)
end
