include	"nlfitdef.h"
include "../lib/nlfit.h"

# NLEVAL -- Evaluate the fit at a point.

real procedure nleval (nl, x, y)

pointer	nl		# nlfit descriptor
real	x		# x value
real	y		# y value

real	zfit

begin
	# Check that the function is defined.
	if (NL_FUNC(nl) == NULL)
	    call error (0, "NLEVAL: Userfnc not set")

	# Evaluate the function.
	call zcall5 (NL_FUNC(nl), x, y, PARAM(NL_PARAM(nl)),
	    NL_NPARAMS(nl), zfit)
	return (zfit)
end
