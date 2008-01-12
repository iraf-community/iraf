include	<math/nlfit.h>
include "nlfitdefd.h"

define	VAR	(($1 - 1) * $2 + 1)

# NLACPTS - Accumulate a series of data points.

double procedure nlacptsd (nl, x, z, w, npts, nvars)

pointer	nl		# pointer to nl fitting structure
double	x[ARB]		# independent variables (npts * nvars)
double	z[ARB]		# function values (npts)
double	w[ARB]		# weights (npts)
int	npts		# number of points
int	nvars		# number of independent variables

int	i, nfree
double	sum, z0, dz

begin
	# Zero the accumulators.
	call aclrd (ALPHA(NL_ALPHA(nl)), NL_NFPARAMS(nl) ** 2)
	call aclrd (BETA(NL_BETA(nl)), NL_NFPARAMS(nl))

	# Accumulate the points into the fit.
	NL_NPTS (nl) = npts
	sum = double(0.0)
	do i = 1, npts {
	    call zcall7 (NL_DFUNC(nl), x[VAR (i, nvars)], nvars,
		PARAM(NL_PARAM(nl)), DPARAM(NL_DELPARAM(nl)), NL_NPARAMS(nl),
		z0, DERIV(NL_DERIV(nl)))
	    dz = z[i] - z0
	    call nl_accumd (DERIV(NL_DERIV(nl)), PLIST(NL_PLIST(nl)),
		w[i], dz, NL_NFPARAMS(nl), ALPHA(NL_ALPHA(nl)), 
		BETA(NL_BETA(nl)))
	    sum = sum + w[i] * dz * dz
	}

	# Return the reduced chisqr.
	nfree = NL_NPTS(nl) - NL_NFPARAMS(nl)
	if (nfree <= 0)
	    return (double (0.0))
	else
	    return (sum / nfree)
end


# NLRESID -- Recompute the residuals

double procedure nlresidd (nl, x, z, w, npts, nvars)

pointer	nl		# pointer to nl fitting structure
double	x[ARB]		# independent variables (npts * nvars)
double	z[ARB]		# function values (npts)
double	w[ARB]		# weights (npts)
int	npts		# number of points
int	nvars		# number of independent variables

int	i, nfree
double	sum, z0, dz

begin
	# Accumulate the residuals.
	NL_NPTS(nl) = npts
	sum = double (0.0)
	do i = 1, npts {
	    call zcall5 (NL_FUNC(nl), x[VAR (i, nvars)], nvars, TRY(NL_TRY(nl)),
		NL_NPARAMS(nl), z0)
	    dz = z[i] - z0
	    sum = sum + dz * dz * w[i]
	}

	# Compute the reduced chisqr.
	nfree = NL_NPTS(nl) - NL_NFPARAMS(nl)
	if (nfree <= 0)
	    return (double (0.0))
	else
	    return (sum / nfree)
end


# NL_ACCUM -- Accumulate a single point into the fit

procedure nl_accumd (deriv, list, w, dz, nfit, alpha, beta)

double	deriv[ARB]	# derivatives
int	list[ARB]	# list of active parameters
double	w		# weight
double	dz		# difference between data and model
int	nfit		# number of fitted parameters
double	alpha[nfit,ARB]	# alpha matrix
double	beta[nfit]	# beta matrix

int	i, j, k
double	wt

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
