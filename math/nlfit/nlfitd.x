include <mach.h>
include <math/nlfit.h>
include "nlfitdefd.h"

# NLFIT -- Routine to perform a non-linear least squares fit. At least MINITER
# iterations must be performed before the fit is complete.

procedure nlfitd (nl, x, z, w, npts, nvars, wtflag, stat)

pointer	nl		# pointer to nlfit structure
double	x[ARB]		# independent variables (npts * nvars)
double	z[ARB]		# function values (npts)
double	w[ARB]		# weights (npts)
int	npts		# number of points
int	nvars		# number of independent variables
int	wtflag		# weighting type
int	stat		# error code

int	i, miniter, ier
double	scatter, dscatter
double	nlscatterd()

begin
	# Initialize.
	NL_ITER(nl) = 0
	NL_LAMBDA(nl) = double (.001)
	NL_REFSQ(nl) = double (0.0)
	NL_SCATTER(nl) = double(0.0)

	# Initialize the weights.
	switch (wtflag) {
	case WTS_UNIFORM:
	    do i = 1, npts
		w[i] = double (1.0)
	case WTS_SCATTER:
	    ;
	case WTS_USER:
	    ;
	case WTS_CHISQ:
	    do i = 1, npts {
		if (z[i] > double (0.0))
		    w[i] = double (1.0) / z[i]
		else if (z[i] < double (0.0))
		    w[i] = double (-1.0) / z[i]
	        else
	            w[i] = double (0.0)
	    }
	default:
	    do i = 1, npts
		w[i] = double (1.0)
	}

	# Initialize.
	scatter = double(0.0)
	if (wtflag == WTS_SCATTER)
	    miniter = MINITER + 1
	else
	    miniter = MINITER

	repeat {

	    # Perform a single iteration.
	    call nliterd (nl, x, z, w, npts, nvars, ier)
	    NL_ITER(nl) = NL_ITER(nl) + 1
	    #call eprintf ("niter=%d refsq=%g oldsq=%g sumsq=%g\n")
		#call pargi (NL_ITER(nl))
		#call parg$t (NL_REFSQ(nl))
		#call parg$t (NL_OLDSQ(nl))
		#call parg$t (NL_SUMSQ(nl))
	    stat = ier
	    if (stat == NO_DEG_FREEDOM)
	        break

	    # Make the convergence checks.
	    if (NL_ITER(nl) < miniter)
		stat = NOT_DONE
	    else if (NL_SUMSQ(nl) <= double (10.0) * EPSILOND)
		stat = DONE
	    else if (NL_REFSQ(nl) < NL_SUMSQ(nl))
		stat = NOT_DONE
	    else if (((NL_REFSQ(nl) - NL_SUMSQ(nl)) / NL_SUMSQ(nl)) <
	        NL_TOL(nl))
		stat = DONE
	    else
		stat = NOT_DONE

	    # Check for a singular solution.
	    if (stat == DONE) {
		if (ier == SINGULAR)
		    stat = ier
		break
	    }

	    # Quit if the lambda parameter goes to zero.
	    if (NL_LAMBDA(nl) <= 0.0)
		break

	    # Check the number of iterations.
	    if ((NL_ITER(nl) >= miniter) && (NL_ITER(nl) >= NL_ITMAX(nl)))
		break

	    # Adjust the weights if necessary.
	    switch (wtflag) {
	    case WTS_SCATTER:
	        dscatter = nlscatterd (nl, x, z, w, npts, nvars)
		if ((NL_ITER(nl) >= MINITER) && (dscatter >= double (10.0) *
		    EPSILOND)) {
		    do i = 1, npts {
			if (w[i] <= double(0.0))
			    w[i] = double(0.0)
			else {
		            w[i] = double(1.0) / (double(1.0) / w[i] + dscatter)
			}
		    }
		    scatter = scatter + dscatter
		}
	    default:
		;
	    }

	    # Get ready for next iteration.
	    NL_REFSQ(nl) = min (NL_OLDSQ(nl), NL_SUMSQ(nl))
	}

	NL_SCATTER(nl) = NL_SCATTER(nl) + scatter
end


# NLSCATTER -- Routine to estimate the original scatter in the fit.

double procedure nlscatterd (nl, x, z, w, npts, nvars)

pointer	nl		# Pointer to nl fitting structure
double	x[ARB]		# independent variables (npts * nvars)
double	z[ARB]		# function values (npts)
double	w[ARB]		# weights (npts)
int	npts		# number of points
int	nvars		# number of independent variables

pointer	sp, zfit, errors
double	scatter, variance, chisqr
int	nlstati()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (zfit, npts, TY_DOUBLE)
	call salloc (errors, nlstati (nl, NLNPARAMS), TY_DOUBLE)

	# Initialize
	scatter = double (0.0)

	# Compute the fit and the errors.
	call nlvectord (nl, x, Memd[zfit], npts, nvars)
	call nlerrorsd (nl, z, Memd[zfit], w, npts, variance, chisqr,
	    Memd[errors])

	# Estimate the scatter.
	if (chisqr <= double(0.0) || variance <= double(0.0))
	    scatter = double (0.0)
	else
	    scatter = double(0.5) * variance * (chisqr - double(1.0)) / chisqr

	call sfree (sp)

	return (scatter)
end
