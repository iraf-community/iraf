include <mach.h>
include "pdm.h"

# PDM_CLOSE -- Close a PDM data structure.

procedure pdm_close (pdmp, interactive)

pointer	pdmp			# PDM structure pointer
bool	interactive		# interactive flag

begin
	# Close icfit pointers, curfit pointers, and graphics pointers,
	# and the ranges pointer.

	call ic_closer (PDM_ICD(pdmp))
	call ic_closer (PDM_ICP(pdmp))
	call cvfree (PDM_CVD(pdmp))
	call cvfree (PDM_CVP(pdmp))
	call gt_free (PDM_GT(pdmp))
	call gclose (PDM_GP(pdmp))
	call rg_free (PDM_RG(pdmp))
	call close (PDM_LFD(pdmp))
	if (!interactive)
	    call close (PDM_PFD(pdmp))

	# Free the data vectors.
	if (PDM_XP(pdmp) != NULL)
	    call mfree (PDM_XP(pdmp), TY_REAL)
	if (PDM_ODYP(pdmp) != NULL)
	    call mfree (PDM_ODYP(pdmp), TY_REAL)
	if (PDM_DYP(pdmp) != NULL)
	    call mfree (PDM_DYP(pdmp), TY_REAL)
	if (PDM_INUSEP(pdmp) != NULL)
	    call mfree (PDM_INUSEP(pdmp), TY_REAL)
	if (PDM_XTHP(pdmp) != NULL)
	    call mfree (PDM_XTHP(pdmp), TY_REAL)
	if (PDM_YTHP(pdmp) != NULL)
	    call mfree (PDM_YTHP(pdmp), TY_REAL)
	if (PDM_YPHP(pdmp) != NULL)
	    call mfree (PDM_YPHP(pdmp), TY_REAL)
	if (PDM_SORTP(pdmp) != NULL)
	    call mfree (PDM_SORTP(pdmp), TY_REAL)
	if (PDM_SAMPLEP(pdmp) != NULL)
	    call mfree (PDM_SAMPLEP(pdmp), TY_CHAR)

	# Free the pdm data structure.
	call mfree (pdmp, TY_STRUCT)
end
