# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "gsurfitdef.h"

define	COV	Memr[P2P($1)]	# element of COV

# GSERRORS -- Procedure to calculate the reduced chi-squared of the fit
# and the standard deviations of the coefficients. First the variance
# and the reduced chi-squared of the fit are estimated. If these two
# quantities are identical the variance is used to scale the errors
# in the coefficients. The errors in the coefficients are proportional
# to the inverse diagonal elements of MATRIX.

procedure gserrors (sf, z, w, zfit, chisqr, errors)

pointer	sf		# curve descriptor
real	z[ARB]		# data points
real	w[ARB]		# array of weights
real	zfit[ARB]	# fitted data points
real	chisqr		# reduced chi-squared of fit
real	errors[ARB]	# errors in coefficients

int	i, nfree
real	variance, chisq, hold
pointer	sp, covptr

begin
	# allocate space for covariance vector
	call smark (sp)
	call salloc (covptr, GS_NCOEFF(sf), TY_REAL)

	# estimate the variance and chi-squared of the fit
	variance = 0.
	chisq = 0.
	do i = 1, GS_NPTS(sf) {
	    hold = (z[i] - zfit[i]) ** 2
	    variance = variance + hold
	    chisq = chisq + hold * w[i]
	}

	# calculate the reduced chi-squared
	nfree = GS_NPTS(sf) - GS_NCOEFF(sf)
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

	do i = 1, GS_NCOEFF(sf) {
	    call aclrr (COV(covptr), GS_NCOEFF(sf))
	    COV(covptr+i-1) = 1.
	    call rgschoslv (CHOFAC(GS_CHOFAC(sf)), GS_NCOEFF(sf),
	        GS_NCOEFF(sf), COV(covptr), COV(covptr))
	    if (COV(covptr+i-1) >= 0.)
	        errors[i] = sqrt (COV(covptr+i-1) * variance)
	    else
		errors[i] = 0.
	}

	call sfree (sp)
end
