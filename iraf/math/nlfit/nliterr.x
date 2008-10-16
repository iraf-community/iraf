include <mach.h>
include	<math/nlfit.h>
include "nlfitdefr.h"

# NLITER - Routine to compute one iteration of the fitting process

procedure nliterr (nl, x, z, w, npts, nvars, ier)

pointer	nl		# pointer to nl fitting structure
real	x[ARB]		# independent variables (npts * nvars)
real	z[ARB]		# function values (npts)
real	w[ARB]		# weights (npts)
size_t	npts		# number of points
int	nvars		# number of independent variables
int	ier		# error code

size_t	sz_val
long	i, index
real	nlacptsr(), nlresidr()

begin
	# Do the initial accumulation.
   	NL_OLDSQ(nl) = nlacptsr (nl, x, z, w, npts, nvars)

	# Set up temporary parameter array.
	sz_val = NL_NPARAMS(nl)
	call amovr (PARAM(NL_PARAM(nl)), TRY(NL_TRY(nl)), sz_val)

	repeat {

	    # Solve the matrix.
	    call nlsolver (nl, ier)
	    if (ier == NO_DEG_FREEDOM)
	        return

	    # Increment the parameters.
	    do i = 1, NL_NFPARAMS(nl) {
	        index = PLIST(NL_PLIST(nl)+i-1)
	        TRY(NL_TRY(nl)+index-1) = TRY(NL_TRY(nl)+index-1) +
		    DPARAM(NL_DPARAM(nl)+i-1)
	    }

	    # Reaccumulate the residuals and increment lambda.
	    NL_SUMSQ(nl) = nlresidr (nl, x, z, w, npts, nvars)
	    #if (NL_OLDSQ(nl) > (NL_SUMSQ(nl) + NL_TOL(nl) * NL_SUMSQ(nl))) {
	    if (NL_OLDSQ(nl) >= NL_SUMSQ(nl)) {
		sz_val = NL_NPARAMS(nl)
		call amovr (TRY(NL_TRY(nl)), PARAM(NL_PARAM(nl)), sz_val)
		NL_LAMBDA(nl) = real (0.10) * NL_LAMBDA(nl)
		break
	    } else
	        NL_LAMBDA(nl) = min (real(LAMBDAMAX),
		    real (10.0) * NL_LAMBDA(nl))

	} until (NL_LAMBDA(nl) <= real(0.0) ||
	    NL_LAMBDA(nl) >= real(LAMBDAMAX))
end
