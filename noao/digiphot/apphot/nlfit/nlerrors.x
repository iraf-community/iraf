include <mach.h>
include "nlfitdef.h"
include "../lib/nlfit.h"

define	COV	Memr[$1]	# element of COV

# NLERRORS -- Procedure to calculate the reduced chi-squared of the fit
# and the errors of the coefficients. First the variance
# and the reduced chi-squared of the fit are estimated. If these two
# quantities are identical the variance is used to scale the errors
# in the coefficients. The errors in the coefficients are proportional
# to the inverse diagonal elements of MATRIX.

procedure nlerrors (nl, wtflag, chisqr, errors)

pointer	nl		# curve descriptor
int	wtflag		# weight flag
real	chisqr		# reduced chi-squared of fit
real	errors[ARB]	# errors in coefficients

int	nfree
pointer	sp, covptr
real   chisq, variance

begin
	# Allocate space for covariance vector.
	call smark (sp)
	call salloc (covptr, NL_NPARAMS(nl), TY_REAL)

	# Compute the chisqr and/or variance.
	switch (wtflag) {
	case WTS_UNIFORM:
	    chisq = NL_SUMSQ(nl)
	    nfree = NL_NPTS(nl) - NL_NFPARAMS(nl)
	    if (nfree  > 0)
	        chisqr = chisq / nfree
	    else
	        chisqr = 0.
	    variance = chisqr
	default:
	    variance = 1.0
	}

	# Calculate the  errors in the coefficients.
	call aclrr (errors, NL_NPARAMS(nl))
	call nlinvert (CHOFAC(NL_CHOFAC(nl)), COV(covptr), errors, 
	    PLIST(NL_PLIST(nl)), NL_NFPARAMS(nl), variance)

	call sfree (sp)
end


# NLINVERT -- Procedure to invert matrix and compute errors

procedure nlinvert (chofac, cov, errors, list, nfit, variance)

real	chofac[nfit, ARB]	# Cholesky factroization
real	cov[ARB]		# covariance vector
real	errors[ARB]		# computed errors
int	list[ARB]		# list of active parameters
int	nfit			# number of fitted parameters
real	variance		# variance of the fit

int	i

begin
	do i = 1, nfit {
	    call aclrr (cov, nfit)
	    cov[i] = 1.0
	    call nl_choslv (chofac, nfit, nfit, cov, cov)
	    if (cov[i] >= 0.)
	        errors[list[i]] = sqrt (cov[i] * variance)
	    else
		errors[list[i]] = 0.
	}
end
