include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

# PDM_PHASE -- Calculate Phase Curve for period (compressed light curve).

procedure pdm_phase (pdmp, period, epoch)

pointer	pdmp			# pointer to PDM data structure
double	period			# period to calculate the phase for
double	epoch			# epoch of this data

int	j, offset, temp
double	p
pointer	npt, phaseint, sp
errchk	calloc, realloc

begin
	call smark (sp)
	npt = PDM_NPT(pdmp)
	call salloc (phaseint, npt, TY_DOUBLE)

	
	# Allocate space for the output phase data (ordinate and abscissa)
	# in the pdm data structure.

	if (PDM_XPHP(pdmp) == NULL) {
	    call calloc (PDM_XPHP(pdmp), 2*npt, TY_DOUBLE)
	    call calloc (PDM_YPHP(pdmp), 2*npt, TY_DOUBLE)
	    call calloc (PDM_PHERRP(pdmp), 2*npt, TY_REAL)
	} else {
	    call realloc (PDM_XPHP(pdmp), 2*npt, TY_DOUBLE)
	    call realloc (PDM_YPHP(pdmp), 2*npt, TY_DOUBLE)
	    call realloc (PDM_PHERRP(pdmp), 2*npt, TY_REAL)
	}

	# Set up the sort array and a temporary array for the phases.
	if (PDM_SORTP(pdmp) == NULL)
	    call calloc (PDM_SORTP(pdmp), npt, TY_INT)
	else
	    call realloc (PDM_SORTP(pdmp), npt, TY_INT)

	# Calculate the phases for all the points.
	for (j=1; j<=npt; j=j+1) {
	    PDM_SORT(pdmp,j) = j
	    if (period > EPSILONR) {
		temp = (int(epoch/period)+1)*period
	        p = (PDM_X(pdmp,j) - epoch + temp)/period
	    }
	    Memd[phaseint+j-1] = double(p - int(p))
	}

	# Sort the phase array into ascending order and permute
	# the index array (sort).

	call pdm_sort (phaseint, PDM_SORTP(pdmp), npt)

	# Store the data in the pdm data structure.
	do j = 1, npt {
	    offset = PDM_SORT(pdmp,j)
	    PDM_YPH(pdmp,j) = PDM_DY(pdmp,offset)
	    PDM_XPH(pdmp,j) = Memd[phaseint+offset-1]
	    PDM_PHERR(pdmp,j) = PDM_ERR(pdmp,offset)
	    PDM_YPH(pdmp,j+npt) = PDM_DY(pdmp,offset)
	    PDM_XPH(pdmp,j+npt) = Memd[phaseint+offset-1] + 1.0
	    PDM_PHERR(pdmp,j+npt) = PDM_ERR(pdmp,offset)
	}

	call sfree (sp)
end
