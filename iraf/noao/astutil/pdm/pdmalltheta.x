include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_ALLTHETA -- Calculate the theta statistic for the period window.

procedure pdm_alltheta (pdmp, porf)

pointer	pdmp				# structure pointer
int	porf				# period or frequency flag

int	i
double	factor, period, pdm_theta(), mmean, mmin, mmax, scale
pointer rg, rg_xrangesd()
errchk	calloc, realloc, rg_xrangesd

begin
	# Check to see that maxp > minp.
	if (PDM_PMAX(pdmp) <= PDM_PMIN(pdmp))
	    call error (0,"maxp smaller or equal minp in alltheta")

	# Calculate the mean of the Y data, remove it.
	mmean = 0.0d+0
	do i = 1, PDM_NPT(pdmp) {
	    mmean = mmean + PDM_DY(pdmp,i)
	}
	mmean = mmean/PDM_NPT(pdmp)
	do i = 1, PDM_NPT(pdmp) {
	    PDM_DY(pdmp,i) = PDM_DY(pdmp,i) - mmean
	}

	# Find the min and max of the data, scale the data 0 to 1.
	mmin = PDM_DY(pdmp,1)
	mmax = PDM_DY(pdmp,1)
	do i = 1, PDM_NPT(pdmp) {
	    if (PDM_DY(pdmp,i) > mmax)
		mmax = PDM_DY(pdmp,i)
	    if (PDM_DY(pdmp,i) < mmin)
		mmin = PDM_DY(pdmp,i)
	}
	scale = mmax - mmin
	do i = 1, PDM_NPT(pdmp) {
	    PDM_DY(pdmp,i) = (PDM_DY(pdmp,i) - mmin)/scale
	}

	# Bring the statistics up to date.
	iferr (call pdm_statistics (pdmp))
	    call error (0, "alltheta: error in statistics")

	# Allocate space for the ordinate and abscissa vectors for theta
	# in the pdm data structure.

	if (PDM_XTHP(pdmp) == NULL) {
	    call calloc (PDM_XTHP(pdmp), PDM_NTHPT(pdmp), TY_DOUBLE)
	    call calloc (PDM_YTHP(pdmp), PDM_NTHPT(pdmp), TY_DOUBLE)
	} else {
	    call realloc (PDM_XTHP(pdmp), PDM_NTHPT(pdmp), TY_DOUBLE)
	    call realloc (PDM_YTHP(pdmp), PDM_NTHPT(pdmp), TY_DOUBLE)
	}

	# Calculate constant for incrementing the period.  Give equally
	# spaced frequencies.

	if (PDM_PMIN(pdmp) <= EPSILOND)
	    if (PDM_NTHPT(pdmp) >= 1)
	        PDM_PMIN(pdmp) = PDM_PMAX(PDMP)/PDM_NTHPT(pdmp)
	    else
		call error (1, "alltheta: num thpts < 1")

	if (PDM_PMIN(pdmp) >= EPSILONR && PDM_NTHPT(pdmp) != 1)
	    factor = (PDM_PMAX(pdmp)/PDM_PMIN(pdmp))**
		(1.0/(PDM_NTHPT(pdmp)-1))

	# Calculate the ranges information from the sample string.
	rg = rg_xrangesd (PDM_SAMPLE(pdmp), PDM_X(pdmp,1), PDM_NPT(pdmp))

	# Call pdm_theta for each period and store the thetas and periods
	# in the pdm data structure, calculate frequencies (1/p) as we go.

	period = PDM_PMIN(pdmp)
	do i = 1, PDM_NTHPT(pdmp) {
	    PDM_YTH(pdmp,i) = pdm_theta (pdmp, rg, period)
	    if (porf == THETAPPLOT)
	        PDM_XTH(pdmp,i) = period
	    else {
	        if (period > EPSILOND)
	            PDM_XTH(pdmp,i) = 1.0d+0/period
		else
		    call error (0, "alltheta: period very close to zero")
	    }
	    period = period * factor
	}

	# Remove the scaling.
	do i = 1, PDM_NPT(pdmp) {
	    PDM_DY(pdmp,i) = (PDM_DY(pdmp,i) * scale) + mmin
	}

	# Put the mean back in.
	do i = 1, PDM_NPT(pdmp) {
	    PDM_DY(pdmp,i) = PDM_DY(pdmp,i) + mmean
	}
end
