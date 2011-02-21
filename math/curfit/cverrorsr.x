# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>

include "curfitdef.h"

define	COV	Memr[P2P($1)]	# element of COV

# CVERRORS -- Procedure to calculate the reduced chi-squared of the fit
# and the standard deviations of the coefficients. First the variance
# and the reduced chi-squared of the fit are estimated. If these two
# quantities are identical the variance is used to scale the errors
# in the coefficients. The errors in the coefficients are proportional
# to the inverse diagonal elements of MATRIX.

procedure cverrors (cv, y, w, yfit, npts, chisqr, errors)

pointer	cv		# curve descriptor
real	y[ARB]		# data points
real	yfit[ARB]	# fitted data points
real	w[ARB]		# array of weights
int	npts		# number of points
real	chisqr		# reduced chi-squared of fit
real	errors[ARB]	# errors in coefficients

int	i, n, nfree
real   variance, chisq, hold
pointer	sp, covptr

begin
	# allocate space for covariance vector
	call smark (sp)
	call salloc (covptr, CV_NCOEFF(cv), TY_REAL)

	# estimate the variance and chi-squared of the fit
	n = 0
	variance = 0.
	chisq = 0.
	do i = 1, npts {
	    if (w[i] <= 0.0)
		next
	    hold = (y[i] - yfit[i]) ** 2
	    variance = variance + hold
	    chisq = chisq + hold * w[i]
	    n = n + 1
	}

	# calculate the reduced chi-squared
	nfree = n - CV_NCOEFF(cv)
	if (nfree  > 0)
	    chisqr = chisq / nfree
	else
	    chisqr = 0.

	# if the variance equals the reduced chi_squared as in the
	# case of uniform weights then scale the errors in the coefficients
	# by the variance not the reduced chi-squared
	if (abs (chisq - variance) <= DELTA)
	    if (nfree > 0)
	        variance = chisq / nfree
	    else
		variance = 0.
	else
	    variance = 1.

	# calculate the  errors in the coefficients
	# the inverse of MATRIX is calculated column by column
	# the error of the j-th coefficient is the j-th element of the
	# j-th column of the inverse matrix
	do i = 1, CV_NCOEFF(cv) {
	    call aclrr (COV(covptr), CV_NCOEFF(cv))
	    COV(covptr+i-1) = 1.
	    call rcvchoslv (CHOFAC(CV_CHOFAC(cv)), CV_ORDER(cv), CV_NCOEFF(cv),
	    	    COV(covptr), COV(covptr))
	    if (COV(covptr+i-1) >= 0.)
	        errors[i] = sqrt (COV(covptr+i-1) * variance)
	    else
		errors[i] = 0.
	}


	call sfree (sp)
end
