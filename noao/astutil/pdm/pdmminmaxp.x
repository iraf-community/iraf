include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_MINMAXP -- Calculate the minimum and maximum periods automatically.

procedure pdm_minmaxp (pdmp)

pointer pdmp			# pointer to PDM data structure

int	npt, i
double	minp, maxp
pointer	sep, sp

begin
	call smark (sp)
	npt = PDM_NPT(pdmp)

	# Allocate an array of separations and fill it.  Find the minimum
	# separation as we go.

	call salloc (sep, npt-1, TY_DOUBLE)
	maxp = PDM_X(pdmp,npt) - PDM_X(pdmp,1)
	minp = maxp
	do i = 1, npt-1 {
	    Memd[sep+i-1] = PDM_X(pdmp,i+1) - PDM_X(pdmp,i)
	    if (Memd[sep+i-1] < minp)
		minp = Memd[sep+i-1]
	}

	# Set minp equal to twice this minimum (Nyquist criterion).  Set fmax.
	PDM_PMIN(pdmp) = 2.0d+0 * minp
	if (PDM_PMIN(pdmp) != 0.0d+0)
	    PDM_FMAX(pdmp) = 1.0d+0/PDM_PMIN(pdmp)

	# Set maxp equal to 4 times maxp.  Set fmin.
	PDM_PMAX(pdmp) = 4.0d+0 * maxp
	if (PDM_PMAX(pdmp) != 0.0d+0)
	    PDM_FMIN(pdmp) = 1.0d+0/PDM_PMAX(pdmp)

	call sfree (sp)
end
