include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

# PDM_FITPHASE -- Call ICFIT on the Phase Curve.

procedure pdm_fitphase (pdmp)

pointer	pdmp			# pointer to PDM data structure

real	xmin, xmax
pointer	weights
int	i, npt
errchk	calloc, icg_addfitr

begin
	# Dereference some pointers.
	npt = PDM_NPT(pdmp)

	# Calloc a phase in-use array.
	call alimr (PDM_XPH(pdmp,1), npt, xmin, xmax)
	call ic_putr (PDM_ICP(pdmp), "xmin", xmin)
	call ic_putr (PDM_ICP(pdmp), "xmax", xmax)
	call calloc (weights, npt, TY_REAL)

	# Permute the data in-use array and save it in the phase weights array.
	do i = 1, npt
	    Memr[weights+i-1] = real(PDM_INUSE(pdmp,PDM_SORT(pdmp,i)))

	# Call icfit on the phase curve (pass the phase x, y, wts)
	if (npt >= 2)
	    call icg_addfitr (PDM_ICP(pdmp), PDM_GP(pdmp), "cursor",
		PDM_GT(pdmp), PDM_CVP(pdmp), PDM_XPH(pdmp,1),
		PDM_YPH(pdmp,1), Memr[weights], npt, npt)

	# Update the data in-use array appropriately.
	do i = 1, npt
	    PDM_INUSE(pdmp,PDM_SORT(pdmp,i)) = Memr[weights+i-1]

	call mfree (weights, TY_REAL)
end
