include <mach.h>
include "pdm.h"

# PDM_CLOSE -- Close a PDM data structure.

procedure pdm_close (pdmp, interactive)

pointer	pdmp			# PDM structure pointer
bool	interactive		# interactive flag

begin
	# Close icfit pointers, curfit pointers, and graphics pointers,
	# and the ranges pointer.

	if (PDM_ICD(pdmp) != NULL)
	    call ic_closed (PDM_ICD(pdmp))
	if (PDM_ICP(pdmp) != NULL)
	    call ic_closed (PDM_ICP(pdmp))
	if (PDM_CVD(pdmp) != NULL)
	    call dcvfree (PDM_CVD(pdmp))
	if (PDM_CVP(pdmp) != NULL)
	    call dcvfree (PDM_CVP(pdmp))
	if (PDM_GT(pdmp) != NULL)
	    call gt_free (PDM_GT(pdmp))
	if (PDM_GP(pdmp) != NULL)
	    call gclose (PDM_GP(pdmp))
	if (PDM_RG(pdmp) != NULL)
	    call rg_free (PDM_RG(pdmp))
	if (PDM_LFD(pdmp) != NULL)
	    call close (PDM_LFD(pdmp))
	if (!interactive)
	    if (PDM_PFD(pdmp) != NULL)
		call close (PDM_PFD(pdmp))

	# Free the data vectors.
	if (PDM_XP(pdmp) != NULL)
	    call mfree (PDM_XP(pdmp), TY_DOUBLE)
	if (PDM_ODYP(pdmp) != NULL)
	    call mfree (PDM_ODYP(pdmp), TY_DOUBLE)
	if (PDM_DYP(pdmp) != NULL)
	    call mfree (PDM_DYP(pdmp), TY_DOUBLE)
	if (PDM_ERRP(pdmp) != NULL)
	    call mfree (PDM_ERRP(pdmp), TY_REAL)
	if (PDM_INUSEP(pdmp) != NULL)
	    call mfree (PDM_INUSEP(pdmp), TY_INT)
	if (PDM_XTHP(pdmp) != NULL)
	    call mfree (PDM_XTHP(pdmp), TY_DOUBLE)
	if (PDM_YTHP(pdmp) != NULL)
	    call mfree (PDM_YTHP(pdmp), TY_DOUBLE)
	if (PDM_XPHP(pdmp) != NULL)
	    call mfree (PDM_XPHP(pdmp), TY_DOUBLE)
	if (PDM_YPHP(pdmp) != NULL)
	    call mfree (PDM_YPHP(pdmp), TY_DOUBLE)
	if (PDM_PHERRP(pdmp) != NULL)
	    call mfree (PDM_PHERRP(pdmp), TY_REAL)
	if (PDM_SORTP(pdmp) != NULL)
	    call mfree (PDM_SORTP(pdmp), TY_INT)
	if (PDM_SAMPLEP(pdmp) != NULL)
	    call mfree (PDM_SAMPLEP(pdmp), TY_CHAR)

	# Free the pdm data structure.
	call mfree (pdmp, TY_STRUCT)
end
