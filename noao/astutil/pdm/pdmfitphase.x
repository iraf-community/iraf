include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

# PDM_FITPHASE -- Call ICFIT on the Phase Curve.

procedure pdm_fitphase (pdmp)

pointer	pdmp			# pointer to PDM data structure

double	xmin, xmax
pointer	weights
int	i, npt
errchk	calloc, icg_fit

begin
	# Dereference some pointers.
	npt = PDM_NPT(pdmp)

	# Calloc a phase in-use array.
	call alimd (PDM_XPH(pdmp,1), npt, xmin, xmax)
	call ic_putr (PDM_ICP(pdmp), "xmin", real(xmin))
	call ic_putr (PDM_ICP(pdmp), "xmax", real(xmax))
	call calloc (weights, npt, TY_DOUBLE)

	# Permute the data in-use array and save it in the phase weights array.
	do i = 1, npt
	    Memd[weights+i-1] = double(PDM_INUSE(pdmp,PDM_SORT(pdmp,i)))

	# Call icfit on the phase curve (pass the phase x, y, wts)
	if (npt >= 2)
	    call icg_fitd (PDM_ICP(pdmp), PDM_GP(pdmp), "cursor",
		PDM_GT(pdmp), PDM_CVP(pdmp), PDM_XPH(pdmp,1),
		PDM_YPH(pdmp,1), Memd[weights], npt)

	# Update the data in-use array appropriately.
	do i = 1, npt
	    PDM_INUSE(pdmp,PDM_SORT(pdmp,i)) = int(Memd[weights+i-1])

	call mfree (weights, TY_DOUBLE)
end
