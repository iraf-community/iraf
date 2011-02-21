include <mach.h>
include <math/nlfit.h>
include "nlfitdefd.h"

define	COV	Memd[P2P($1)]	# element of COV


# NLERRORS -- Procedure to calculate the reduced chi-squared of the fit
# and the errors of the coefficients. First the variance
# and the reduced chi-squared of the fit are estimated. If these two
# quantities are identical the variance is used to scale the errors
# in the coefficients. The errors in the coefficients are proportional
# to the inverse diagonal elements of MATRIX.

procedure nlerrorsd (nl, z, zfit, w, npts, variance, chisqr, errors)

pointer	nl		# curve descriptor
double	z[ARB]		# data points
double	zfit[ARB]	# fitted data points
double	w[ARB]		# array of weights
int	npts		# number of points
double	variance	# variance of the fit
double	chisqr		# reduced chi-squared of fit (output)
double	errors[ARB]	# errors in coefficients (output)

int	i, n, nfree
pointer	sp, covptr
double   factor

begin
	# Allocate space for covariance vector.
	call smark (sp)
	call salloc (covptr, NL_NPARAMS(nl), TY_DOUBLE)

	# Estimate the variance and reduce chi-squared of the fit.
	n = 0
	variance = double (0.0)
	chisqr = double (0.0)
	do i = 1, npts {
	    if (w[i] <= double (0.0))
	        next
	    factor = (z[i] - zfit[i]) ** 2
	    variance = variance + factor
	    chisqr = chisqr + factor * w[i]
	    n = n + 1
	}

	# Calculate the reduced chi-squared.
	nfree = n - NL_NFPARAMS(nl)
	if (nfree  > 0) {
	    variance = variance / nfree
	    chisqr = chisqr / nfree
	} else {
	    variance = double (0.0)
	    chisqr = double (0.0)
	}

	# If the variance equals the reduced chi_squared as in the case
	# of uniform weights then scale the errors in the coefficients
	# by the variance and not the reduced chi-squared

	if (abs (chisqr - variance) <= EPSILOND) {
	    if (nfree > 0)
		factor = chisqr
	    else
		factor = double (0.0)
	} else
	    factor = 1.

	# Calculate the  errors in the coefficients.
	call aclrd (errors, NL_NPARAMS(nl))
	call nlinvertd (ALPHA(NL_ALPHA(nl)), CHOFAC(NL_CHOFAC(nl)),
	    COV(covptr), errors, PLIST(NL_PLIST(nl)), NL_NFPARAMS(nl), factor)

	call sfree (sp)
end


# NLINVERT -- Procedure to invert matrix and compute errors

procedure nlinvertd (alpha, chofac, cov, errors, list, nfit, variance)

double	alpha[nfit,ARB]		# data matrix
double	chofac[nfit, ARB]	# cholesky factorization
double	cov[ARB]		# covariance vector
double	errors[ARB]		# computed errors
int	list[ARB]		# list of active parameters
int	nfit			# number of fitted parameters
double	variance		# variance of the fit

int	i, ier

begin
	# Factorize the data matrix to determine the errors.
	call nl_chfacd (alpha, nfit, nfit, chofac, ier)

	# Estimate the errors.
	do i = 1, nfit {
	    call aclrd (cov, nfit)
	    cov[i] = double (1.0)
	    call nl_chslvd (chofac, nfit, nfit, cov, cov)
	    if (cov[i] >= double (0.0))
	        errors[list[i]] = sqrt (cov[i] * variance)
	    else
		errors[list[i]] = double (0.0)
	}
end
