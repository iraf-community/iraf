# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	"mwcs.h"

# IW_SETAXMAP -- If the reference image was opened with an image section,
# modify the Lterm to reflect the section transformation, and enable the
# axis map if any dimensional reduction was involved.

procedure iw_setaxmap (mw, im)

pointer	mw			#I pointer to MWCS descriptor
pointer	im			#I pointer to reference image

double	v
int	ndim, i, j
pointer	sp, ltv_1, ltv_2, ltm
int	axno[MAX_DIM], axval[MAX_DIM]

begin
	# If there is no section we don't need to do anything.
	if (IM_SECTUSED(im) == NO)
	    return

	call smark (sp)

	ndim = IM_NPHYSDIM(im)
	call salloc (ltv_1, ndim, TY_DOUBLE)
	call salloc (ltv_2, ndim, TY_DOUBLE)
	call salloc (ltm, ndim*ndim, TY_DOUBLE)

	# The section transformation is  px = VSTEP * lx + VOFF, specifying
	# the transformation from logical to physical coordinates.  The IMIO
	# axis map is given by j=VMAP[i], mapping logical axis I to physical
	# axis J.  Hence the physical to logical transformation in terms of
	# IMIO units is given by  lx = (1/VSTEP) * px + (-VOFF/VSTEP).  Since
	# the section transform forbids rotation the axes are independent.

	call aclrd (Memd[ltv_1], ndim)
	call aclrd (Memd[ltm], ndim * ndim)

	do i = 1, ndim {
	    if (IM_VSTEP(im,i) == 0)
		v = 1.0D0
	    else
		v = 1.0D0 / IM_VSTEP(im,i)

	    Memd[ltm+(i-1)*ndim+i-1] = v
	    Memd[ltv_2+(i-1)] = -(IM_VOFF(im,i) * v)
	}

	# Enter the section transformation.
	call mw_translated (mw, Memd[ltv_1], Memd[ltm], Memd[ltv_2], ndim)

	# Set the axis map.
	do j = 1, ndim {
	    for (i=1;  i <= IM_NDIM(im);  i=i+1)
		if (IM_VMAP(im,i) == j)
		    break
	    if (i > IM_NDIM(im)) {
		axno[j] = 0
		axval[j] = IM_VOFF(im,j)
	    } else {
		axno[j] = i
		axval[j] = 0
	    }
	}
	    
	call mw_saxmap (mw, axno, axval, ndim)
	call sfree (sp)
end
