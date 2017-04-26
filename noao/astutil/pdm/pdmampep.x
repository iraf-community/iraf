include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_AMPEP -- Calculate the amplitude and epoch for this data.

procedure pdm_ampep (pdmp, period)

pointer	pdmp			# PDM structure pointer
double	period			# period for which to calculate

int	i, isave
double	npt, ymin, ymax
errchk	pdm_phase

begin
	npt = PDM_NPT(pdmp)

	# Find the maximum and minimum values in the data.
	# The difference is the amplitude.

	ymax = -MAX_DOUBLE
	ymin = MAX_DOUBLE
	do i = 1, npt {
	    if (PDM_INUSE(pdmp,i) == 0)
		next
	    if (PDM_DY(pdmp,i) < ymin)
		ymin = PDM_DY(pdmp,i)
	    if (PDM_DY(pdmp,i) > ymax) {
		ymax = PDM_DY(pdmp,i)
		isave = i
	    }
	}

	PDM_AMPL(pdmp) = ymax - ymin
	PDM_EPOCH(pdmp) = PDM_X(pdmp, isave)
end
