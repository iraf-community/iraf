include <mach.h>
include	<math/nlfit.h>
include "nlfitdefd.h"

# NLITER - Routine to compute one iteration of the fitting process

procedure nliterd (nl, x, z, w, npts, nvars, ier)

pointer	nl		# pointer to nl fitting structure
double	x[ARB]		# independent variables (npts * nvars)
double	z[ARB]		# function values (npts)
double	w[ARB]		# weights (npts)
int	npts		# number of points
int	nvars		# number of independent variables
int	ier		# error code

int	i, index
double	nlacptsd(), nlresidd()

begin
	# Do the initial accumulation.
   	NL_OLDSQ(nl) = nlacptsd (nl, x, z, w, npts, nvars)

	# Set up temporary parameter array.
	call amovd (PARAM(NL_PARAM(nl)), TRY(NL_TRY(nl)), NL_NPARAMS(nl))

	repeat {

	    # Solve the matrix.
	    call nlsolved (nl, ier)
	    if (ier == NO_DEG_FREEDOM)
	        return

	    # Increment the parameters.
	    do i = 1, NL_NFPARAMS(nl) {
	        index = PLIST(NL_PLIST(nl)+i-1)
	        TRY(NL_TRY(nl)+index-1) = TRY(NL_TRY(nl)+index-1) +
		    DPARAM(NL_DPARAM(nl)+i-1)
	    }

	    # Reaccumulate the residuals and increment lambda.
	    NL_SUMSQ(nl) = nlresidd (nl, x, z, w, npts, nvars)
	    #if (NL_OLDSQ(nl) > (NL_SUMSQ(nl) + NL_TOL(nl) * NL_SUMSQ(nl))) {
	    if (NL_OLDSQ(nl) >= NL_SUMSQ(nl)) {
		call amovd (TRY(NL_TRY(nl)), PARAM(NL_PARAM(nl)),
		    NL_NPARAMS(nl))
		NL_LAMBDA(nl) = double (0.10) * NL_LAMBDA(nl)
		break
	    } else
	        NL_LAMBDA(nl) = min (double(LAMBDAMAX),
		    double (10.0) * NL_LAMBDA(nl))

	} until (NL_LAMBDA(nl) <= double(0.0) ||
	    NL_LAMBDA(nl) >= double(LAMBDAMAX))
end
