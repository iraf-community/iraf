include <mach.h>
include	"nlfitdef.h"
include	"../lib/nlfit.h"

# NLITER - Routine to compute one iteration of the fitting process

procedure nliter (nl, x, y, z, w, nx, ny, nz, wtflag, ier)

pointer	nl		# pointer to nl fitting structure
real	x[nx]		# array of x ordinates
real	y[ny]		# array of y ordinates
real	z[nx,nz]	# data array
real	w[nx,nz]	# array of weights
int	nx, ny, nz	# number of points
int	wtflag		# type of weighting
int	ier		# error code

int	i, index
real	nlacpts(), nlresid()

begin
	# Do the initial accumulation.
   	NL_OLDSQ(nl) = nlacpts (nl, x, y, z, w, nx, ny, nz, wtflag)

	# Set up temporary parameter array.
	call amovr (PARAM(NL_PARAM(nl)), TRY(NL_TRY(nl)), NL_NPARAMS(nl))

	repeat {

	    # Solve the matrix.
	    call nlsolve (nl, ier)
	    if (ier == NO_DEG_FREEDOM)
	        return

	    # Increment the parameters.
	    do i = 1, NL_NFPARAMS(nl) {
	        index = PLIST(NL_PLIST(nl)+i-1)
	        TRY(NL_TRY(nl)+index-1) = TRY(NL_TRY(nl)+index-1) +
		    DPARAM(NL_DPARAM(nl)+i-1)
	    }

	    # Reaccumulate the residuals and increment lambda.
	    NL_SUMSQ(nl) = nlresid (nl, x, y, z, w, nx, ny, nz, wtflag)
	    if ((NL_OLDSQ(nl) + NL_TOL(nl) * NL_OLDSQ(nl)) > NL_SUMSQ(nl)) {
		call amovr (TRY(NL_TRY(nl)), PARAM(NL_PARAM(nl)),
		    NL_NPARAMS(nl))
		NL_LAMBDA(nl) = 0.10 * NL_LAMBDA(nl)
		break
	    } else
	        NL_LAMBDA(nl) = 10.0 * NL_LAMBDA(nl)

	} until (NL_LAMBDA(nl) > LAMBDAMAX)

end
