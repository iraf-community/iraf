include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_STATISTICS -- Calculate the sum of squares and the variance of the data.

procedure pdm_statistics (pdmp)

pointer	pdmp			# pointer to PDM data structure

int	npt, i, j
double	var, sumx2, sum

begin
	npt = PDM_NPT(pdmp)

	# Calculate the sum of squares and the variance of the data.
	sumx2 = 0.0
	sum = 0.0
	j = 0

	do i = 1, npt {
	    if (PDM_INUSE(pdmp,i) == 1) {
	        sumx2 = sumx2 + PDM_DY(pdmp,i)**2	    # Sum of squares.
	        sum = sum + PDM_DY(pdmp,i)
		j = j + 1
	    }
	}

	if (j != 1)
	    var = (sumx2 - sum**2/double(j))/double(j - 1)    # Variance.

	# Put these two values in the data structure.
	PDM_SUMSQ(pdmp) = sumx2
	PDM_DVAR(pdmp) = var
end
