include <mach.h>
include	"nlfitdef.h"
include "../lib/nlfit.h"

# NLFIT -- Routine to perform a non-linear least squares fit

procedure nlfit (nl, x, y, z, w, nx, ny, nz, wtflag, stat)

pointer	nl		# Pointer to nl fitting structure
real	x[nx]		# X ordinates
real	y[ny]		# Y ordinates
real	z[nx,nz]	# Data array
real	w[nx,nz]	# Weights
int	nx, ny, nz	# Number of points
int	wtflag		# Weighting stat
int	stat		# Error code

int	i, j,ier

begin
	NL_ITER(nl) = 0
	NL_LAMBDA(nl) = .001
	NL_REFSQ(nl) = 0.0

	# Compute the weights.
	if (nz == 1 && nx == ny) {

	    switch (wtflag) {
	    case WTS_UNIFORM, WTS_SPACING:
		do i = 1, nx
		    w[i,1] = 1.0
	    case WTS_USER:
		# do nothing
	    case WTS_CHISQ:
		do i = 1, nx {
		    if (z[i,1] > 0.0)
			w[i,1] = 1.0 / z[i,1]
		    else if (z[i,1] < 0.0)
			w[i,1] = - 1.0 / z[i,1]
		    else
			w[i,1] = 0.0
		}
	    default:
		do i = 1, nx
		    w[i,1] = 1.0
	    }

	} else if (nz == ny) {

	    switch (wtflag) {
	    case WTS_UNIFORM, WTS_SPACING:
		do j = 1, ny
		    do i = 1, nx
			w[i,j] = 1.0
	    case WTS_USER:
		# do nothing
	    case WTS_CHISQ:
		do j = 1, ny
		    do i = 1, nx {
			if (z[i,j] > 0.0)
			    w[i,j] = 1.0 / z[i,j]
			else if (z[i,j] < 0.0)
			    w[i,j] = - 1.0 / z[i,j]
			else
			    w[i,j] = 0.0
		    }
	    default:
		do j = 1, ny
		    do i = 1, nx
			w[i,j] = 1.0
	    }

	} else
	    call error ( 0, "NLACPTS: Illegal nx, ny, or nz values" )

	while (NL_ITER(nl) < NL_ITMAX(nl)) {

	    # Perform a single iteration.
	    call nliter (nl, x, y, z, w, nx, ny, nz, wtflag, ier)
	    NL_ITER(nl) = NL_ITER(nl) + 1
	    if (ier == NO_DEG_FREEDOM)
	        break

	    # Make the convergence checks.
	    if (NL_SUMSQ(nl) <= 100.0 * EPSILONR)
		stat = DONE
	    else if (NL_REFSQ(nl) < NL_SUMSQ(nl))
		stat = NOT_DONE
	    else if ((NL_REFSQ(nl) - NL_SUMSQ(nl)) < NL_SUMSQ(nl) *
		NL_TOL(nl))
		stat = DONE
	    else
		stat = NOT_DONE
	    if (stat == DONE)
		break

	    NL_REFSQ(nl) = NL_SUMSQ(nl)
	}
end
