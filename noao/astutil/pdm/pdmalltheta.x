include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_ALLTHETA -- Calculate the theta statistic for the period window.

procedure pdm_alltheta (pdmp, porf)

pointer	pdmp				# structure pointer
int	porf				# period or frequency flag

int	i
real	factor, period, pdm_theta()
pointer rg, rg_xrangesr()
errchk	calloc, realloc, rg_xrangesr

begin
	# Check to see that maxp > minp.
	if (PDM_PMAX(pdmp) <= PDM_PMIN(pdmp))
	    call error (0,"maxp smaller or equal minp in alltheta")

	# Bring the statistics up to date.
	iferr (call pdm_statistics (pdmp))
	    call error (0, "alltheta: error in statistics")

	# Allocate space for the ordinate and abscissa vectors for theta
	# in the pdm data structure.

	if (PDM_XTHP(pdmp) == NULL) {
	    call calloc (PDM_XTHP(pdmp), PDM_NTHPT(pdmp), TY_REAL)
	    call calloc (PDM_YTHP(pdmp), PDM_NTHPT(pdmp), TY_REAL)
	} else {
	    call realloc (PDM_XTHP(pdmp), PDM_NTHPT(pdmp), TY_REAL)
	    call realloc (PDM_YTHP(pdmp), PDM_NTHPT(pdmp), TY_REAL)
	}

	# Calculate constant for incrementing the period.  Give equally
	# spaced frequencies.

	if (PDM_PMIN(pdmp) <= EPSILONR)
	    if (PDM_NTHPT(pdmp) >= 1)
	        PDM_PMIN(pdmp) = PDM_PMAX(PDMP)/PDM_NTHPT(pdmp)
	    else
		call error (1, "alltheta: num thpts < 1")

	if (PDM_PMIN(pdmp) >= EPSILONR && PDM_NTHPT(pdmp) != 1)
	    factor = (PDM_PMAX(pdmp)/PDM_PMIN(pdmp))**
		(1.0/(PDM_NTHPT(pdmp)-1))

	# Calculate the ranges information from the sample string.
	rg = rg_xrangesr (PDM_SAMPLE(pdmp), PDM_X(pdmp,1), PDM_NPT(pdmp))

	# Call pdm_theta for each period and store the thetas and periods
	# in the pdm data structure, calculate frequencies (1/p) as we go.

	period = PDM_PMIN(pdmp)
	do i = 1, PDM_NTHPT(pdmp) {
	    PDM_YTH(pdmp,i) = pdm_theta (pdmp, rg, period)
	    if (porf == THETAPPLOT)
	        PDM_XTH(pdmp,i) = period
	    else {
	        if (period > EPSILONR)
	            PDM_XTH(pdmp,i) = 1./period
		else
		    call error (0, "alltheta: period very close to zero")
	    }
	    period = period * factor
	}
end
