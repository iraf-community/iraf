# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_SHIFT -- Front end to mw_translate, used to perform a simple shift
# of the logical system.

procedure mw_shift (mw, shift, axbits)

pointer	mw			#I pointer to MWCS descriptor
real	shift[ARB]		#I shift for each axis in axbits
int	axbits			#I bitflags defining axes

pointer	sp, ltm, ltv_1, ltv_2
int	axis[MAX_DIM], naxes, pdim, nelem, axmap, i

begin
	# Convert axis bitflags to axis list.
	call mw_gaxlist (mw, axbits, axis, naxes)
	if (naxes <= 0)
	    return

	pdim = MI_NDIM(mw)
	nelem = pdim * pdim
	axmap = MI_USEAXMAP(mw)
	MI_USEAXMAP(mw) = NO

	call smark (sp)
	call salloc (ltm, nelem, TY_DOUBLE)
	call salloc (ltv_1, pdim, TY_DOUBLE)
	call salloc (ltv_2, pdim, TY_DOUBLE)

	# Initialize the translation matrix and vectors.
	call mw_mkidmd (Memd[ltm], pdim)
	call aclrd (Memd[ltv_1], pdim)
	call aclrd (Memd[ltv_2], pdim)

	# Enter the axis shifts.
	do i = 1, naxes
	    Memd[ltv_2+axis[i]-1] = shift[i]

	# Perform the translation.
	call mw_translated (mw, Memd[ltv_1], Memd[ltm], Memd[ltv_2], pdim)

	MI_USEAXMAP(mw) = axmap
	call sfree (sp)
end
