include <mach.h>
include <ctype.h>
include <error.h>
include	<gset.h>
include <pkg/rg.h>
include "pdm.h"

define	NUMTRIES	100

# PDM_SIGNIF -- Calculate the significance of the theta statistic for
# a certain period.  Use the "Method of Randomization".

double procedure pdm_signif (pdmp, period)

pointer	pdmp			# pointer to PDM data structure
double	period			# period at which to find significance

int	lesscount, i, npt
double	otheta, theta, pdm_theta(), pdm_thetaran()
long	seed
pointer rg, pt, inuse, oinuse, rg_xrangesd(), sp
errchk	pdm_statistics, pdm_theta(), pdm_ranperm, pdm_thetaran()

begin
        # Do NUMTRIES random permutations on the data, calculate the theta
        # statistic on the scrambled data for this period.  Return the
        # fraction of these permutaions which yield thetas less than
        # the unmixed data.

	call smark (sp)
	npt = PDM_NPT(pdmp)

	# Make sure the statistics are up to date.
	call pdm_statistics (pdmp)

	# Allocate a temporary array for the scrambled data, allocate a 
	# temporary copy of the inuse array and copy the real inuse array
	# into it.

	call salloc (pt, npt, TY_DOUBLE)
	call salloc (inuse, npt, TY_INT)
	call salloc (oinuse, npt, TY_INT)
	call amovi (PDM_INUSE(pdmp,1), Memi[inuse], npt)
	lesscount = 0

	# Calculate the ranges information from the sample string.
	rg = rg_xrangesd (PDM_SAMPLE(pdmp), PDM_X(pdmp,1), npt)
	otheta = pdm_theta (pdmp, rg, period)
	seed = 1.0

	do i = 1, NUMTRIES {
	    call pdm_ranperm (PDM_DY(pdmp,1), Memi[inuse], Memd[pt],
		Memi[oinuse], npt, seed)
	    theta = pdm_thetaran (pdmp, pt, oinuse, rg, period)
	    if (theta < otheta)
		lesscount = lesscount + 1
	}

	call sfree (sp)
	return (1.0d+0 - (double(lesscount)/double(NUMTRIES)))
end
