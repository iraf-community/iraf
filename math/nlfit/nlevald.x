include <math/nlfit.h>
include "nlfitdefd.h"


# NLEVAL -- Evaluate the fit at a point.

double procedure nlevald (nl, x, nvars)

pointer	nl		# nlfit descriptor
double	x[ARB]		# x values
int	nvars		# number of variables

real	zfit

begin
	# Evaluate the function.
	call zcall5 (NL_FUNC(nl), x, nvars, PARAM(NL_PARAM(nl)),
	    NL_NPARAMS(nl), zfit)

	return (zfit)
end
