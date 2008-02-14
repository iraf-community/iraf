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
pointer	sp, ltv_1, ltv_2, ltm
int	wcsdim, ndim, physax, i, j
int	axno[MAX_DIM], axval[MAX_DIM]
int	o_axno[MAX_DIM], o_axval[MAX_DIM]
int	n_axno[MAX_DIM], n_axval[MAX_DIM]

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
	# the transformation from logical to physical image coordinates.
	# The IMIO axis map is given by j=VMAP[i], mapping logical axis I to
	# physical axis J.  Hence the physical to logical transformation in
	# terms of IMIO units is given by  lx = (1/VSTEP) * px + (-VOFF/VSTEP).
	# Since the section transform forbids rotation the axes are independent.

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

	# Enter the section transformation.  This uses the axis map, but the
	# transformation is defined in terms of the physical image matrix,
	# which is defined by the old axis map before modification by the new
	# image section.  Hence we must do this step before editing the axis
	# map below.

	call mw_translated (mw, Memd[ltv_1], Memd[ltm], Memd[ltv_2], ndim)

	# Compute the axis map for the active image section relative to the
	# current physical image matrix.

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

	# Get the old axis map for the WCS.  In the general case the WCS can
	# have a dimension higher than the current image, i.e. if the current
	# image was produced by extracting a section of an image of higher
	# dimension.  In such a case the WCS will have an axis map relating
	# the physical axes of the current image back to the original physical
	# system.

	wcsdim = MI_NDIM(mw)
	call mw_gaxmap (mw, o_axno, o_axval, wcsdim)

	# Combine the old axis map and the axis map for the current image
	# section.  The old axis map physical->logical mapping maps WCS
	# physical axes to logical axes, which are the physical axes of the
	# current image.  The axis map for the current image section maps the
	# physical axes of the current image to the logical axes of the
	# section.  An axis removed in the WCS axis map is not visible in the
	# image axno/axval computed above; the corresponding axis in the
	# combined WCS axis map is unchanged.  The remaining axes are subject
	# to remapping by the mage axno/axval.  This mapping may set any of
	# the axes to a constant to further reduce the dimensionality of the
	# logical system, however that does not concern us here, we just pass
	# on the combined axno/axval vectors to mw_saxmap.

        do i = 1, wcsdim {
            if (o_axno[i] == 0) {
                n_axno[i] = 0
                n_axval[i] = o_axval[i]
            } else {
                physax = o_axno[i]
                n_axno[i] = axno[physax]
                n_axval[i] = axval[physax]
            }
	}

	# Set the new axis map.
	call mw_saxmap (mw, n_axno, n_axval, wcsdim)

	call sfree (sp)
end
