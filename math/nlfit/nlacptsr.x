include	<math/nlfit.h>
include "nlfitdefr.h"

define	VAR	(($1 - 1) * $2 + 1)

# NLACPTS - Accumulate a series of data points.

real procedure nlacptsr (nl, x, z, w, npts, nvars)

pointer	nl		# pointer to nl fitting structure
real	x[ARB]		# independent variables (npts * nvars)
real	z[ARB]		# function values (npts)
real	w[ARB]		# weights (npts)
int	npts		# number of points
int	nvars		# number of independent variables

int	i, nfree
real	sum, z0, dz

begin
	# Zero the accumulators.
	call aclrr (ALPHA(NL_ALPHA(nl)), NL_NFPARAMS(nl) ** 2)
	call aclrr (BETA(NL_BETA(nl)), NL_NFPARAMS(nl))

	# Accumulate the points into the fit.
	NL_NPTS (nl) = npts
	sum = real(0.0)
	do i = 1, npts {
	    call zcall7 (NL_DFUNC(nl), x[VAR (i, nvars)], nvars,
		PARAM(NL_PARAM(nl)), DPARAM(NL_DELPARAM(nl)), NL_NPARAMS(nl),
		z0, DERIV(NL_DERIV(nl)))
	    dz = z[i] - z0
	    call nl_accumr (DERIV(NL_DERIV(nl)), PLIST(NL_PLIST(nl)),
		w[i], dz, NL_NFPARAMS(nl), ALPHA(NL_ALPHA(nl)), 
		BETA(NL_BETA(nl)))
	    sum = sum + w[i] * dz * dz
	}

	# Return the reduced chisqr.
	nfree = NL_NPTS(nl) - NL_NFPARAMS(nl)
	if (nfree <= 0)
	    return (real (0.0))
	else
	    return (sum / nfree)
end


# NLRESID -- Recompute the residuals

real procedure nlresidr (nl, x, z, w, npts, nvars)

pointer	nl		# pointer to nl fitting structure
real	x[ARB]		# independent variables (npts * nvars)
real	z[ARB]		# function values (npts)
real	w[ARB]		# weights (npts)
int	npts		# number of points
int	nvars		# number of independent variables

int	i, nfree
real	sum, z0, dz

begin
	# Accumulate the residuals.
	NL_NPTS(nl) = npts
	sum = real (0.0)
	do i = 1, npts {
	    call zcall5 (NL_FUNC(nl), x[VAR (i, nvars)], nvars, TRY(NL_TRY(nl)),
		NL_NPARAMS(nl), z0)
	    dz = z[i] - z0
	    sum = sum + dz * dz * w[i]
	}

	# Compute the reduced chisqr.
	nfree = NL_NPTS(nl) - NL_NFPARAMS(nl)
	if (nfree <= 0)
	    return (real (0.0))
	else
	    return (sum / nfree)
end


# NL_ACCUM -- Accumulate a single point into the fit

procedure nl_accumr (deriv, list, w, dz, nfit, alpha, beta)

real	deriv[ARB]	# derivatives
int	list[ARB]	# list of active parameters
real	w		# weight
real	dz		# difference between data and model
int	nfit		# number of fitted parameters
real	alpha[nfit,ARB]	# alpha matrix
real	beta[nfit]	# beta matrix

int	i, j, k
real	wt

begin
	do i = 1, nfit {
	    wt = deriv[list[i]] * w
	    k = 1
	    do j = i, nfit {
	        alpha[k,i] = alpha[k,i] + wt * deriv[list[j]]
		k = k + 1
	    }
	    beta[i] = beta[i] + dz * wt
	}
end
