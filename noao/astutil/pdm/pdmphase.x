include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

# PDM_PHASE -- Calculate Phase Curve for period (compressed light curve).

procedure pdm_phase (pdmp, period, epoch)

pointer	pdmp			# pointer to PDM data structure
real	period			# period to calculate the phase for
real	epoch			# epoch of this data

int	j, offset, temp
real	p
pointer	npt, phaseint, sp
errchk	calloc, realloc

begin
	call smark (sp)
	npt = PDM_NPT(pdmp)
	call salloc (phaseint, npt, TY_REAL)
	
	# Allocate space for the output phase data (ordinate and abscissa)
	# in the pdm data structure.

	if (PDM_XPHP(pdmp) == NULL) {
	    call calloc (PDM_XPHP(pdmp), 2*npt, TY_REAL)
	    call calloc (PDM_YPHP(pdmp), 2*npt, TY_REAL)
	} else {
	    call realloc (PDM_XPHP(pdmp), 2*npt, TY_REAL)
	    call realloc (PDM_YPHP(pdmp), 2*npt, TY_REAL)
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
	    Memr[phaseint+j-1] = real(p - int(p))
	}

	# Sort the phase array into ascending order and permute
	# the index array (sort).

	call pdm_sort (phaseint, PDM_SORTP(pdmp), npt)

	# Store the data in the pdm data structure.
	do j = 1, npt {
	    offset = PDM_SORT(pdmp,j)
	    PDM_YPH(pdmp,j) = PDM_DY(pdmp,offset)
	    PDM_XPH(pdmp,j) = Memr[phaseint+offset-1]
	    PDM_YPH(pdmp,j+npt) = PDM_DY(pdmp,offset)
	    PDM_XPH(pdmp,j+npt) = Memr[phaseint+offset-1] + 1.0
	}

	call sfree (sp)
end
